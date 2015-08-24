{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler {-(
    compiler
  )-}
  where

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

import Control.Arrow    (first, second)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe       (fromJust)
import Data.List        (sortBy, mapAccumR)
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

--------------------------------------------------------------------------------
-- * Channels
--------------------------------------------------------------------------------

data Hide f where
  Hide :: f a -> Hide f

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

initC   :: forall i. CompileExp (IExp i) => Rim.Map (Linked i) -> Channels i
initC m = let links = fmap snd . concat $ Rim.dump m in
    snd $ flip execState (1, Map.empty) $ forM_ links add
  where
    add :: Rim.HideType (Linked i) -> Init i ()
    add (Rim.Hide (Linked _ l@(Link out))) = insert l out

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

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

type M i = ReaderT (Rim.Map (Linked i)) (StateT (Channels i) (Program i))

node :: Name (S Symbol i a) -> M i (Linked i (S Symbol i a))
node name =
  do out <- ask
     return $ case Rim.lookup name out of
       Nothing   -> error "Compiler.node: lookup failed"
       Just node -> node

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
  do (Linked n olink@(Link out)) <- node sym
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
       (Delay v ilink@(Link s)) ->   -- ! This is clearly wrong
         do e <- readE ilink s
            writeE olink out v
       _ -> return ()
  where
    down :: Stream i a -> M i a
    down = lift . lift . Str.run

    up   :: forall proxy i a. proxy i a -> U i a -> M i (Stream i (U i a))
    up _ = return . Str.Stream . return . return 

--------------------------------------------------------------------------------

compile'
  :: forall i a.
     ( ConcurrentCMD (IExp i) :<: i
     , CompileExp (IExp i)
     , PredicateExp (IExp i) a
     , Typeable a
     )
  => Key i (Identity a)
  -> Rim.Map (Node i)
  -> Rim.Map (Linked i)
  -> [Ordered i]
  -> Str i a
compile' k@(Key root) nodes links order = Stream $ 
  do let ch = initC links
     run ch $ do
       mapM_ comp' order
       out <- readE k (name root)
       ret out
  where
    run :: Channels i -> M i (IExp i a) -> Program i (Program i (IExp i a))
    run c = return . flip evalStateT c . flip runReaderT links

    ret :: U i (Identity a) -> M i (IExp i a)
    ret = return

--------------------------------------------------------------------------------

compiler
  :: ( ConcurrentCMD (IExp i) :<: i
     , CompileExp (IExp i)
     , PredicateExp (IExp i) a
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
       False -> compile' root nodes links order

--------------------------------------------------------------------------------
