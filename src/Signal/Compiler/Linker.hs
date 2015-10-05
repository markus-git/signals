{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler.Linker (
    Dist
  , Named (..)
  , Names    
  , Link  (..)
  , Linked(..)
  , Links    
  , linker
  , name
  )
  where

import Signal.Core
import Signal.Core.Reify
import Signal.Core.Witness
import Signal.Compiler.Knot
import Signal.Compiler.Sorter
import Signal.Compiler.Linker.Names

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.Hashable
import Data.Ref
import Data.Ref.Map (Map, Name)
import Unsafe.Coerce

import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right, Ordering)

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------
-- Once we have names for every wire (Names), we can substitute the old
-- group names

-- | Nodes where recursive calls to other nodes have been replaced with names
data Link   (i :: (* -> *) -> * -> *) (a :: *)
  where
    Link :: Witness i a => Names (S Symbol i a) -> Link i a

-- | Container for linked nodes and the names of their own output
data Linked (i :: (* -> *) -> * -> *) (a :: *)
  where
    Linked :: S Link i a -> Link i a -> Linked i (S Symbol i a)

-- | ...
type Links i = Map (Linked i)

--------------------------------------------------------------------------------
-- ** We will however need to hide their types as they vary between nodes

data Hide f where
  Hide :: f a -> Hide f

data Pair f g a where
  Pair :: f a -> g a -> Pair f g a

type Item i = Hide (Pair Name (Linked i))

--------------------------------------------------------------------------------
-- ** Linking monad

type Resolution i = Links i

type Constraint i = Item  i

type M i          = Knot (Resolution i) (Constraint i) (State (Nodes i))

--------------------------------------------------------------------------------

node :: Name (S Symbol i a) -> M i (Node i (S Symbol i a))
node name =
  do out <- get
     return $ case M.lookup name out of
       Nothing   -> error "Linker.node:lookup failed"
       Just node -> node

resolve :: Key i a -> M i (Link i a)
resolve (Key name) =
  do out <- ask
     return $ case M.lookup name out of
       Nothing           -> error "Linker.resolve:lookup failed"
       Just (Linked _ l) -> l

output :: Item i -> M i ()
output i = tell [i]

--------------------------------------------------------------------------------
-- * Linker
--------------------------------------------------------------------------------

link' :: Ordered i -> M i ()
link' (Ordered sym) =
  do (Node n) <- node sym
     case n of
       (Var d) ->
         do constrain (Var d) (name sym)
       (Repeat c) ->
         do constrain (Repeat c) (name sym)
       (Map f s) ->
         do inp <- resolve s
            constrain (Map f inp) (name sym)
       (Join l r) ->
         do inp_l <- resolve l
            inp_r <- resolve r
            constrain (Join inp_l inp_r) (reify inp_l, reify inp_r)
       (Delay c s) ->
         do inp <- resolve s
            constrain (Delay c inp) (name sym)
       (Mux s choices) ->
         do inp  <- resolve s
            inps <- forM choices $ \(c, s) ->
                do s' <- resolve s
                   return (c, s')
            constrain (Mux inp inps) (name sym)
  where
    reify ~(Link n) = n
    constrain n l   = output $ Hide $ Pair sym $ Linked n $ Link l

--------------------------------------------------------------------------------

linker :: [Ordered i] -> Nodes i -> Links i
linker order nodes = snd . flip evalState nodes . tie solve $ forM_ order link'
  where
    solve :: Solver (Resolution i) (Constraint i)
    solve = foldr ins M.empty

    ins :: Item i -> Links i -> Links i
    ins (Hide (Pair n l)) = M.insert (Data.Ref.Ref n undefined) l

--------------------------------------------------------------------------------
