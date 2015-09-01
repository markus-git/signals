{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler (compiler) where

import Control.Monad.Operational.Compositional

import Signal.Core hiding (lift)
import Signal.Core.Stream 
import Signal.Core.Reify
import qualified Signal.Core        as S
import qualified Signal.Core.Stream as Str

import Signal.Compiler.Cycles
import Signal.Compiler.Linker
import Signal.Compiler.Sorter

import Language.VHDL          hiding (Name)
import Language.Embedded.VHDL hiding (name)
import Language.Embedded.VHDL.Monad (Type)

import Control.Arrow    (first, second)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.Either      (partitionEithers)
import Data.Maybe       (catMaybes)
import Data.List        (sortBy, delete)
import Data.Traversable (traverse)
import Data.Typeable
import Data.Function    (on)
import Data.Hashable
import Data.Constraint
import Data.Ref
import Data.Ref.Map     (Name)
import qualified Data.IntMap  as Map
import qualified Data.Ref.Map as Rim

import Unsafe.Coerce -- !

import Prelude hiding (lookup, Left, Right)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

type Channels (i :: (* -> *) -> * -> *) = Map.IntMap Identifier

lookupC :: Named (S Symbol i (Identity a)) -> Channels i -> Identifier
lookupC n m = case Map.lookup (hash n) m of
  Nothing -> error "Compiler.lookupC: lookup failed"
  Just i  -> i

lookupE
  :: (CompileExp (IExp i), PredicateExp (IExp i) a)
  => Named (S Symbol i (Identity a))
  -> Channels i
  -> IExp i a
lookupE n = varE . lookupC n

insertC :: Named (S Symbol i (Identity a)) -> Identifier -> Channels i -> Channels i
insertC n i m = Map.insert (hash n) i m

--------------------------------------------------------------------------------

type Init i = State (Integer, Channels i)

new :: Init i Identifier
new = 
  do i <- gets fst
     modify $ first (+ 1)
     return $ newVar i

insert
  :: forall proxy i a. (CompileExp (IExp i), Witness i a)
  => proxy i a
  -> Names (S Symbol i a)
  -> Init i ()
insert _ n = go (wit :: Wit i a) n
  where
    go :: Wit i x -> Names (S Symbol i x) -> Init i ()
    go (WE)     (name) = add name
    go (WP u v) (l, r) = go u l >> go v r

    add :: PredicateExp (IExp i) x => Named (S Symbol i (Identity x)) -> Init i ()
    add name = new >>= modify . second . insertC name

initC   :: forall i. CompileExp (IExp i) => Rim.Map (Linked i) -> Channels i
initC m = let links = fmap snd . concat $ Rim.dump m in
    snd $ flip execState (1, Map.empty) $ forM_ links add
  where
    add :: Rim.HideType (Linked i) -> Init i ()
    add (Rim.Hide (Linked s l@(Link out)))
      | isUseful s = insert l out
      | otherwise  = return ()

    isUseful :: S sym i a -> Bool
    isUseful (Join _ _) = False
    isUseful (Left   _) = False
    isUseful (Right  _) = False
    isUseful _          = True

--------------------------------------------------------------------------------
-- **

type M i = ReaderT (Rim.Map (Linked i)) (StateT (Channels i) (Program i))

readE
  :: forall proxy i a. (ConcurrentCMD (IExp i) :<: i, CompileExp (IExp i), Witness i a)
  => proxy i a
  -> Names (S Symbol i a)
  -> M i (U i a)
readE _ n = go (wit :: Wit i a) n
  where
    go :: Wit i x -> Names (S Symbol i x) -> M i (U i x)
    go (WE)     (name) = gets $ lookupE name
    go (WP u v) (l, r) =
      do l' <- go u l
         r' <- go v r
         return (l', r')

writeE
  :: forall proxy i a. (ConcurrentCMD (IExp i) :<: i, Witness i a)
  => proxy i a
  -> Names (S Symbol i a)
  -> U i a
  -> M i ()
writeE _ n e = go (wit :: Wit i a) n e
  where
    go :: Wit i x -> Names (S Symbol i x) -> U i x -> M i ()
    go (WP u v) (l, r) (a, b) = go u l a >> go v r b
    go (WE)     (name) (expr) =
      do c <- gets $ lookupC name
         lift $ lift $ c <== expr

--------------------------------------------------------------------------------

comp' :: (ConcurrentCMD (IExp i) :<: i, CompileExp (IExp i)) => Ordered i -> M i ()
comp' (Ordered sym) =
  do (Linked n olink@(Link out)) <- asks (Rim.! sym)
     case n of
       (Repeat c) ->
         do v <- down c
            writeE olink out v
            
       (Map f ilink@(Link s)) ->
         do e <- readE ilink s
            v <- up    ilink e
            let o = f v
            y <- down o
            writeE olink out y
            
       (Delay _ ilink@(Link s)) ->
         do e <- readE ilink s
            writeE olink out e
            
       _ -> return ()
  where
    down :: Stream i a -> M i a
    down = lift . lift . Str.run

    up   :: forall proxy i a. proxy i a -> U i a -> M i (Stream i (U i a))
    up _ = return . Str.Stream . return . return 

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

compile'
  :: forall i a.
     ( ConcurrentCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) a
     , Typeable a
     )
  => Key i (Identity a)
  -> Rim.Map (Linked i)
  -> [Ordered i]
  -> Str i a
compile' k@(Key root) links order = Stream $ 
  do init
     initRoot
     run $ do
       let (delays, nodes) = splitDelays
       mapM_ comp' nodes
       mapM_ comp' delays
       readE k (name root)
  where
    ch :: Channels i
    ch = initC links
    
    run :: M i (IExp i a) -> Program i (Program i (IExp i a))
    run = return . flip evalStateT ch . flip runReaderT links

    init :: Program i ()
    init = forM_ (Ordered root `delete` order) $ \(Ordered n) -> case Rim.lookup n links of
      Nothing -> error "compile'.initD: sorter appears to have added an extra node"
      Just (Linked (Delay v _) (Link o)) -> signalL (lookupC o ch) (Just v)
      Just (Linked (Repeat v)  (Link o)) -> initNode o
      Just (Linked (Map _ _)   (Link o :: Link i z)) -> dist (wit :: Wit i z) o
        where
          dist :: Wit i x -> Names (S Symbol i x) -> Program i ()
          dist (WP u v) (l, r) = dist u l >> dist v r
          dist (WE)     (name) = initNode name
      _ -> return ()

    initNode :: forall i x.
         (ConcurrentCMD (IExp i) :<: i, Typeable x)
      => Named (S Symbol i (Identity x))
      -> Program  i ()
    initNode name = signalL (lookupC name ch) (Nothing :: Maybe (IExp i x))

    initRoot :: Program i ()
    initRoot = void $ signal (lookupC (name root) ch) Out (Nothing :: Maybe (IExp i a))

    splitDelays :: ([Ordered i], [Ordered i])
    splitDelays = partitionEithers $ flip fmap order $ \o@(Ordered n) -> case Rim.lookup n links of
      Nothing -> error "compile'.isDelay: sorter appears to have added an extra node"
      Just (Linked (Delay _ _) _) -> P.Left  o
      Just _                      -> P.Right o

--------------------------------------------------------------------------------

compiler
  :: ( ConcurrentCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) a
     , Typeable a
     )
  => Sig i a -> IO (Str i a)
compiler sig =
  do (root, nodes) <- reify sig

     let order = sorter root  nodes
         cycle = cycles root  nodes
         links = linker order nodes
     
     return $ case cycle of
       True  -> error "Compiler.compiler: found cycle"
       False -> compile' root links order

--------------------------------------------------------------------------------
