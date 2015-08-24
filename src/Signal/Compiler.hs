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

import Language.VHDL
import Language.Embedded.VHDL

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

-- ! generalize ! Expr a --> IExp i a

data Hide f where
  Hide :: f a -> Hide f

data Channels (i :: (* -> *) -> * -> *) = Channels (Map.IntMap (Hide (IExp i)))

lookupC :: Named (S Symbol i (Identity a)) -> Channels i -> IExp i a
lookupC n (Channels m) = case Map.lookup (hash n) m of
  Nothing       -> error "Compiler.lookupC: lookup failed"
  Just (Hide x) -> unsafeCoerce x

insertC :: Named (S Symbol i (Identity a)) -> IExp i a -> Channels i -> Channels i
insertC n e (Channels m) = Channels $ Map.insert (hash n) (Hide e) m

--------------------------------------------------------------------------------

type Init i = State (Integer, Channels i)

new :: (CompileExp (IExp i), PredicateExp (IExp i) a) => Init i (IExp i a)
new = 
  do i <- gets fst
     modify $ first (+ 1)
     return $ newVar i
{-
insert
  :: forall proxy i a. (CompileExp (IExp i), Witness a)
  => proxy i a
  -> Names (S Symbol i a)
  -> Init i ()
insert _ n = go (wit :: Wit a) n
  where
    go :: Wit x -> Names (S Symbol i x) -> Init i ()
    go (WE)     (name) = add name
    go (WP u v) (l, r) = go u l >> go v r

    add :: PredicateExp (IExp i) x => Named (S Symbol i (Identity x)) -> Init i ()
    add name = new >>= \i -> modify $ second $ insertC name i
-}{-
chan_init   :: Rim.Map (Linked i) -> Channels
chan_init m =
  let links = fmap snd . concat $ Rim.dump m
   in snd $ flip execState (1, Map.empty) $ forM_ links add
  where
    add :: Rim.HideType (Linked i) -> Init ()
    add (Rim.Hide (Linked _ l@(Link out))) = insert l out
-}
--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------
{-
type M i = ReaderT (Rim.Map (Linked i)) (StateT Channels (Program i))

node :: Name (S Symbol i a) -> M i (Linked i (S Symbol i a))
node name =
  do out <- ask
     return $ case Rim.lookup name out of
       Nothing   -> error "Compiler.node: lookup failed"
       Just node -> node

read_exp  :: forall proxy i a. (IExp i ~ Expr, Witness a)
          => proxy i a
          -> Names (S Symbol i a)
          -> M i (U i a)
read_exp _ n = go (wit :: Wit a) n
  where
    go :: Wit x -> Names (S Symbol i x) -> M i (U i x)
    go (WE)     (name) = gets $ chan_lookup name
    go (WP u v) (l, r) =
      do l' <- go u l
         r' <- go v r
         return (l', r')

write_exp :: forall proxy i a. (ConcurrentCMD (IExp i) :<: i, IExp i ~ Expr, Witness a)
          => proxy i a
          -> Names (S Symbol i a)
          -> U i a
          -> M i ()
write_exp _ n e = go (wit :: Wit a) n e
  where
    go :: Wit x -> Names (S Symbol i x) -> U i x -> M i ()
    go (WP u v) (l, r) (a, b) = go u l a >> go v r b
    go (WE)     (name) (expr) =
      do c <- gets $ chan_find name
         lift $ lift $ c <=: expr
-}
--------------------------------------------------------------------------------
{-
comp' :: (ConcurrentCMD (IExp i) :<: i, IExp i ~ Expr) => Ordered i -> M i ()
comp' (Ordered sym) =
  do (Linked n olink@(Link out)) <- node sym
     case n of
       (Repeat c) ->
         do v <- down c
            write_exp olink out v
       (Map f ilink@(Link s)) ->
         do e <- read_exp ilink s
            v <- up       ilink e
            let o = f v
            y <- down o
            write_exp olink out y
       (Delay v ilink@(Link s)) ->   -- ! This is clearly wrong
         do e <- read_exp ilink s
            write_exp olink out v
       _ -> return ()
  where
    down :: Stream i a -> M i a
    down = lift . lift . Str.run

    up   :: forall proxy i a. proxy i a -> U i a -> M i (Stream i (U i a))
    up _ = return . Str.Stream . return . return 
-}    
--------------------------------------------------------------------------------
{-
compile' :: forall i a. (ConcurrentCMD (IExp i) :<: i, IExp i ~ Expr, Typeable a)
         => Key i (Identity a)
         -> Rim.Map (Node i)
         -> Rim.Map (Linked i)
         -> [Ordered i]
         -> Str i a
compile' k@(Key root) nodes links order = Stream $ 
  do let ch = chan_init links
     run ch $ do
       mapM_ comp' order
       out <- read_exp k (name root)
       ret out
  where
    run :: Channels -> M i (Expr a) -> Program i (Program i (Expr a))
    run c = return . flip evalStateT c . flip runReaderT links

    ret :: U i (Identity a) -> M i (Expr a)
    ret = return
-}
--------------------------------------------------------------------------------
{-
compiler :: (ConcurrentCMD (IExp i) :<: i, IExp i ~ Expr, Typeable a)
         => Sig i a
         -> IO (Str i a)
compiler sig =
  do (root, nodes) <- reify sig

     let order = sorter root  nodes
         cycle = cycles root  nodes
         links = linker order nodes
     
     return $ case cycle of
       True  -> error "Compiler.compiler: found cycle"
       False -> compile' root nodes links order
-}
--------------------------------------------------------------------------------
