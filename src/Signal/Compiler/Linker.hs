{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler.Linker (
    Link  (..)
  , Linked(..)
  , Links    
  , linker

  , Hide(..)

    -- ^ re-export of naming constructs
  , module Signal.Compiler.Linker.Names
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
import qualified Data.Ref.Map as M

import Unsafe.Coerce

import Prelude hiding (Left, Right, Ordering)

--------------------------------------------------------------------------------
-- * Linking Types
--------------------------------------------------------------------------------
-- Once we have names for every wire (Names), we can substitute the old
-- group names

-- | Nodes where recursive calls to other nodes have been replaced with names
data Link   (i :: (* -> *) -> * -> *) (a :: *) where
  Link :: Witness i a => Names (S Symbol i a) -> Link i a

-- | Container for linked nodes and the names of their own output
data Linked (i :: (* -> *) -> * -> *) (a :: *) where
  Linked :: S Link i a -> Link i a -> Linked i (S Symbol i a)

-- | Short-hand for a mapping over links
type Links i = Map (Linked i)

--------------------------------------------------------------------------------
-- ** We will however need to hide their types as they vary between nodes

-- | A single constructor with a hidden type parameter
data Hide f where
  Hide :: f a -> Hide f

-- | A pair of constructors applied to the same type parameter
data Pair f g a where
  Pair :: f a -> g a -> Pair f g a

-- | Two hidden constructors
type Item i = Hide (Pair Name (Linked i))

--------------------------------------------------------------------------------
-- ** Linking monad

type Resolution i = Links i

type Constraint i = Item  i

type M i          = Knot (Resolution i) (Constraint i) (State (Nodes i))

--------------------------------------------------------------------------------
-- some helper functions

-- | Find the named node
node :: Name (S Symbol i a) -> M i (Node i (S Symbol i a))
node name = gets (M.! name)

-- | Find the indexed key
resolve :: Key i a -> M i (Link i a)
resolve (Key name) = asks ((\(Linked _ l) -> l) . (M.! name))

-- | Tell a new output item
output :: Item i -> M i ()
output i = tell [i]

--------------------------------------------------------------------------------
-- * Linker
--------------------------------------------------------------------------------

-- | Resolves inputs and constrains the output of each node
link' :: Ordered i -> M i ()
link' (Ordered (Key sym)) =
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

-- | Link together ordered nodes, results in a mapping over their connections
linker :: [Ordered i] -> Nodes i -> Links i
linker order nodes = snd . flip evalState nodes . tie solve $ forM_ order link'
  where
    solve :: Solver (Resolution i) (Constraint i)
    solve = foldr ins M.empty

    ins :: Item i -> Links i -> Links i
    ins (Hide (Pair n l)) = M.insert (Data.Ref.Ref n undefined) l

--------------------------------------------------------------------------------
