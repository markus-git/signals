{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Compiler.Sorter (
    Order
  , sorter
  )
where

import Frontend.Signal hiding (lift)
  
import Control.Arrow
import Control.Monad.State
import Data.Ref
import Data.Unique
import Data.List (sort)

import           Data.Map (Map, (!))
import qualified Data.Map as M

import Prelude hiding (Left, Right)

--------------------------------------------------------------------------------
-- * Sorter
--------------------------------------------------------------------------------

-- | During the sorting process a node can either be sorted or unvisited 
data Status   = Visited | Unvisited deriving Eq

-- | The ordering assigned to a node after being sorted
type Order    = Int

-- | ...
type Mapping  = Map Unique (Status, Order)

-- | ...
type M = State (Order, Mapping)

--------------------------------------------------------------------------------
-- **

-- | Returns a new and unique ordering
new :: M Order
new = do
  (i, m) <- get
  put (i + 1, m)
  return i

--------------------------------------------------------------------------------
-- **

-- | Updates the order of a node
tag :: Unique -> Order -> M ()
tag u o = modify $ second $ flip M.adjust u $ \(s, _) -> (s, o)

-- | Updates the status of a node
mark :: Unique -> Status -> M ()
mark u s = modify $ second $ M.insert u (s, undefined)

-- | Gets the status of a node
status :: Unique -> M Status
status u = gets (fst . (! u) . snd)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | Given a root and a set of graph nodes, a topological ordering is produced
sorter :: Sig i a -> [(Unique, Order)]
sorter (Sig (Signal sym)) =
  fmap (fmap snd) $ M.toList $ snd $ flip execState (0, M.empty) $ sort' sym

--------------------------------------------------------------------------------

-- | Standard depth-first ordering of a graph
sort' :: Symbol i a -> M ()
sort' (Symbol (Ref u sym)) = do
    u `mark` Visited
    b <- case sym of
       (Repeat  _) -> return ()
       (Map   _ s) -> visit s
       (Join  l r) -> visit l >> visit r
       (Left    s) -> visit s
       (Right   s) -> visit s
       (Delay _ s) -> visit s
    o <- new
    tag u o
  where
    visit :: Symbol i a -> M ()
    visit sym@(Symbol (Ref u _)) = do
      v <- status u
      when (v /= Visited) $ sort' sym

--------------------------------------------------------------------------------
