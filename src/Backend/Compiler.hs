{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Compiler {-(
    compiler
  )-}
  where

import Core
import qualified Core as C

import Frontend.Stream 
import Frontend.Signal hiding (lift)
import Frontend.Signal.Observ
import qualified Frontend.Stream as Str
import qualified Frontend.Signal as S
import Backend.Compiler.Cycles
import Backend.Compiler.Linker hiding (M)
import Backend.Compiler.Sorter
import Backend.VHDL.CMD
import Backend.VHDL.Expr   (Expr( Var ))
import Backend.VHDL.Syntax (Identifier(..))
import Backend.VHDL.Generate

import Control.Arrow (first, second)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe       (fromJust)
import Data.List        (sortBy, mapAccumR)
import Data.Traversable (traverse)
import Data.Function    (on)
import Data.Hashable
import Data.Constraint
import Data.Ref
import Data.Ref.Map (Name)
import qualified Data.IntMap  as Map
import qualified Data.Ref.Map as Rim

import Unsafe.Coerce
import Prelude hiding (lookup, Left, Right)

--------------------------------------------------------------------------------
-- * Channels
--------------------------------------------------------------------------------

type Channels = Map.IntMap Identifier

chan_find   :: Typeable a => Named (S Symbol i (Identity a)) -> Channels -> Identifier
chan_find   n c = let (Var i) = chan_lookup n c in i

chan_lookup :: Typeable a => Named (S Symbol i (Identity a)) -> Channels -> Expr a
chan_lookup n c = case Map.lookup (hash n) c of
  Nothing -> error "Compiler.chan_lookup: lookup failed"
  Just i  -> Var i

chan_insert :: Named a -> Identifier -> Channels -> Channels
chan_insert n = Map.insert (hash n)

--------------------------------------------------------------------------------

type Init = State (Int, Channels)

new :: Init Identifier
new =
  do i <- gets fst
     modify $ first (+ 1)
     return $ Ident $ 's' : show i

insert :: forall proxy i a. Witness a => proxy i a -> Names (S Symbol i a) -> Init ()
insert _ n = go (wit :: Wit a) n
  where
    go :: Wit x -> Names (S Symbol i x) -> Init ()
    go (WE)     (name) = add name
    go (WP u v) (l, r) = go u l >> go v r

    add :: Named x -> Init ()
    add name = new >>= \i -> modify $ second $ chan_insert name i

chan_init   :: Rim.Map (Linked i) -> Channels
chan_init m =
  let links = fmap snd . concat $ Rim.dump m
   in snd $ flip execState (1, Map.empty) $ forM_ links add
  where
    add :: Rim.HideType (Linked i) -> Init ()
    add (Rim.Hide (Linked _ l@(Link out))) = insert l out

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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
    
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
