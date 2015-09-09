{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler (compiler, compiler_fun) where

import Control.Monad.Operational.Compositional

import Signal.Core hiding (lift)
import Signal.Core.Stream 
import Signal.Core.Reify
import qualified Signal.Core        as S
import qualified Signal.Core.Stream as Str

import Signal.Compiler.Cycles
import Signal.Compiler.Linker
import Signal.Compiler.Sorter

import Language.VHDL    (Identifier(..))
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

import Prelude hiding (lookup, Left, Right)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Channels
--------------------------------------------------------------------------------

type Channels = Map.IntMap Identifier

lookupC :: Named (S Symbol i (Identity a)) -> Channels -> Identifier
lookupC n m = case Map.lookup (hash n) m of
  Nothing -> error "Compiler.lookupC: lookup failed"
  Just i  -> i

insertC :: Named (S Symbol i (Identity a)) -> Identifier -> Channels -> Channels
insertC n i m = Map.insert (hash n) i m

--------------------------------------------------------------------------------
-- ** Initialization of Channels

type Init = State (Integer, Channels)

new :: Init Identifier
new = 
  do i <- gets fst
     modify $ first (+ 1)
     return $ newVar i

initC   :: forall i. CompileExp (IExp i) => Rim.Map (Linked i) -> Channels
initC m = let links = fmap snd . concat $ Rim.dump m in
    snd $ flip execState (1, Map.empty) $ mapM_ add links
  where
    add :: Rim.HideType (Linked i) -> Init ()
    add (Rim.Hide (Linked s l@(Link out)))
      | isUseful s = insert l out
      | otherwise  = return ()

    isUseful :: S sym i a -> Bool
    isUseful (Join _ _) = False
    isUseful (Left   _) = False
    isUseful (Right  _) = False
    isUseful _          = True

    insert :: forall a. Witness i a => Link i a -> Names (S Symbol i a) -> Init ()
    insert _ n = go (wit :: Wit i a) n
      where
        go :: Wit i x -> Names (S Symbol i x) -> Init ()
        go (WE)     (name) = new >>= modify . second . insertC name
        go (WP u v) (l, r) = go u l >> go v r

--------------------------------------------------------------------------------
-- ** Reading / Writing to and from Channels

type M i = ReaderT (Rim.Map (Linked i), Channels) (Program i)

readE
  :: forall proxy i a. (CompileExp (IExp i), Witness i a)
  => proxy i a
  -> Names (S Symbol i a)
  -> M i (U i a)
readE _ n = go (wit :: Wit i a) n
  where
    go :: Wit i x -> Names (S Symbol i x) -> M i (U i x)
    go (WE)     (name) = asks $ varE . lookupC name . snd
    go (WP u v) (l, r) =
      do l' <- go u l
         r' <- go v r
         return (l', r')

writeS
  :: forall proxy i a. (SequentialCMD (IExp i) :<: i, Witness i a)
  => proxy i a
  -> Names (S Symbol i a)
  -> U i a
  -> M i ()
writeS _ n e = go (wit :: Wit i a) n e
  where
    go :: Wit i x -> Names (S Symbol i x) -> U i x -> M i ()
    go (WP u v) (l, r) (a, b) = go u l a >> go v r b
    go (WE)     (name) (expr) =
      do c <- asks $ lookupC name . snd
         lift $ c <== expr

writeV
  :: forall proxy i a. (SequentialCMD (IExp i) :<: i, Witness i a)
  => proxy i a
  -> Names (S Symbol i a)
  -> U i a
  -> M i ()
writeV _ n e = go (wit :: Wit i a) n e
  where
    go :: Wit i x -> Names (S Symbol i x) -> U i x -> M i ()
    go (WP u v) (l, r) (a, b) = go u l a >> go v r b
    go (WE)     (name) (expr) =
      do c <- asks $ lookupC name . snd
         lift $ c ==: expr

-- *** need writeV, for variable assignment

--------------------------------------------------------------------------------
-- * Compilation
--------------------------------------------------------------------------------

compiler
  :: ( ConcurrentCMD (IExp i) :<: i
     , SequentialCMD (IExp i) :<: i
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

compiler_fun
  :: ( ConcurrentCMD (IExp i) :<: i
     , SequentialCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) a
     , PredicateExp  (IExp i) b
     , Typeable i
     , Typeable a
     , Typeable b
     )
  => (Sig i a -> Sig i b) -> IO (Str i a -> Str i b)
compiler_fun sf =
  do (root, input, nodes) <- reify_fun sf
     
     let order = sorter root  nodes
         cycle = cycles root  nodes
         links = linker order nodes
     
     return $ case cycle of
       True  -> error "Compiler.compiler: found cycle"
       False -> compile'_fun root input links order

--------------------------------------------------------------------------------
-- **

compile'
  :: forall i a.
     ( ConcurrentCMD (IExp i) :<: i
     , SequentialCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) a
     , Typeable a
     )
  => Key i (Identity a)
  -> Links i
  -> [Ordered i]
  -> Str i a
compile' k@(Key root) links order = Stream $ 
  do signal (lookupC (name root) channels) Out (Nothing :: Maybe (IExp i a))
     compile'_init channels links orders
     compile'_run  channels links $ do
       -- network
       wrap $ do
         let r = name root
         mapM_ (comp' k) nodes
         mapM_ (comp' k) delays
       -- output
       readE k (name root)
  where
    channels        = initC links    
    (orders)        = foldr delete order [Ordered root]
    (delays, nodes) = compile'_split channels links order

    -- Wraps a 'M' action in a process
    wrap :: M i () -> M i ()
    wrap = lift . process (Ident "main") [] . flip runReaderT (links, channels)

--------------------------------------------------------------------------------

compile'_fun
  :: forall i a b.
     ( ConcurrentCMD (IExp i) :<: i
     , SequentialCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) a
     , PredicateExp  (IExp i) b
     , Typeable i
     , Typeable a
     , Typeable b
     )
  => Key i (Identity b)
  -> Key i (Identity a)
  -> Links i
  -> [Ordered i]
  -> (Str i a -> Str i b)
compile'_fun outp@(Key root) inp@(Key var) links order (Stream str) = Stream $ 
  do next <- str
     signal (lookupC (name var)  channels) In  (Nothing :: Maybe (IExp i a))
     signal (lookupC (name root) channels) Out (Nothing :: Maybe (IExp i b))
     compile'_init channels links orders
     compile'_run  channels links $ do
       -- input
       val <- lift $ next
       -- network
       wrap $ do
         writeS inp (name var) val
         mapM_ (comp' outp) nodes
         mapM_ (comp' outp) delays
       -- output
       readE outp (name root)
  where
    channels        = initC links    
    (orders)        = foldr delete order [Ordered root, Ordered var]
    (delays, nodes) = compile'_split channels links order

    -- Wraps a 'M' action in a process
    wrap :: M i () -> M i ()
    wrap = lift . process (Ident "main") [lookupC (name var) channels]
                . flip runReaderT (links, channels)

--------------------------------------------------------------------------------
-- **

comp'
  :: forall i a. (SequentialCMD (IExp i) :<: i, CompileExp (IExp i), Witness i a, Typeable a)
  => Key i a    -- root node of signal graph
  -> Ordered i  -- node to compile
  -> M i ()
comp' (Key root) node@(Ordered sym) =
  do (Linked n olink@(Link out)) <- asks $ (Rim.! sym) . fst
     case n of
       (Repeat c) ->
         do v <- down c
            write olink out v
       (Map f ilink@(Link s)) ->
         do e <- readE ilink s
            v <- up    ilink e
            let o = f v
            y <- down o
            write olink out y            
       (Delay _ ilink@(Link s)) ->
         do e <- readE ilink s
            write olink out e
       _ -> return ()
  where
    -- there must be a better way than this..
    write :: forall proxy b. Witness i b => proxy i b -> Names (S Symbol i b) -> U i b -> M i ()
    write | (Ordered root) == node = writeS
          | otherwise              = writeV

    down :: Stream i b -> M i b
    down = lift . Str.run

    up   :: proxy i b -> U i b -> M i (Stream i (U i b))
    up _ = return . Str.Stream . return . return 

--------------------------------------------------------------------------------
-- **

type Order i = [Ordered i]

-- | Declare variable instances for each node in 'order'
compile'_init
  :: forall i. (SequentialCMD (IExp i) :<: i)
  => Channels
  -> Links i
  -> Order i
  -> Program i ()
compile'_init channels links order = forM_ order $ \(Ordered n) ->
  case Rim.lookup n links of
    Nothing -> error "Compiler.compile'_init: lookup failed"
    Just (Linked (Delay v _) (Link o)) -> init o (Just v)
    Just (Linked (Repeat  v) (Link o)) -> init o (Nothing)
    Just (Linked (Map   _ _) (Link o :: Link i x)) -> dist (wit :: Wit i x) o
      where
        dist :: Wit i a -> Names (S Symbol i a) -> Program i ()
        dist (WP u v) (l, r) = dist u l >> dist v r
        dist (WE)     (name) = init name (Nothing)        
    _ -> return ()
  where
    init
      :: forall i a. (SequentialCMD (IExp i) :<: i, Typeable a)
      => Named (S Symbol i (Identity a))
      -> Maybe (IExp i a)
      -> Program i ()
    init n v = variableL (lookupC n channels) v

-- | Filter out all delay nodes from 'order'
compile'_split
  :: Channels
  -> Links i
  -> Order i
  -> ( Order i -- delays
     , Order i -- not delays
     )
compile'_split channels links order =
    partitionEithers
  $ flip fmap order
  $ \o@(Ordered n) -> case Rim.lookup n links of
      Just (Linked (Delay _ _) _) -> P.Left  o
      Just _                      -> P.Right o

-- | Runs the 'M' monad wrapper and returns its program
compile'_run
  :: Channels
  -> Links i
  -> M i (IExp i a)
  -> Program i (Program i (IExp i a))
compile'_run channels links = return . flip runReaderT (links, channels)

--------------------------------------------------------------------------------
