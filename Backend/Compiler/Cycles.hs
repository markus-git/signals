module Backend.Compiler.Cycles (
    cycles
  )
where

import Frontend.Signal.Observ

import Control.Monad.State
import Data.Reify (Graph(..), Unique, reifyGraph)
import Data.Map   (Map, (!))

import qualified Data.Map as M

import Prelude hiding (pred, cycle)

--------------------------------------------------------------------------------
-- * Cycles
--------------------------------------------------------------------------------

-- | A node can have three different states during cycle checking
--   * Visited,   no cycles detected in node or children
--   * Visiting,  node is being checked for cycles
--   * Unvisited, node has not yet been checked for cycles
data Status = Visited | Visiting | Unvisited deriving Eq

-- | A node's predecessor
type Predecessor = Unique

--------------------------------------------------------------------------------

-- | Updates the status for a node
mark :: Unique -> Status -> State (Map Unique (Status, p, n)) ()
mark u s = modify $ flip M.adjust u $ \(_, p, n) -> (s, p, n)

-- | Updates the predecessor for a node
pred :: Unique -> Predecessor -> State (Map Unique (s, Predecessor, n)) ()
pred u p = modify $ flip M.adjust u $ \(s, _, n) -> (s, p, n)

-- | Gets the status of a node
status :: Unique -> State (Map Unique (Status, p, n)) Status
status u = get >>= return . (\(s, _, _) -> s) . (! u)

-- | Gets the predecessor of a node
predecessor :: Unique -> State (Map Unique (s, Predecessor, n)) Predecessor
predecessor u = get >>= return . (\(_, p, _) -> p) . (! u)

-- | Gets the adjacent nodes of a node
adjacent :: Unique -> State (Map Unique (s, p, Node e)) [Unique]
adjacent u = get >>= return . (\(_, _, n) -> edges' n) . (! u)
  where
    -- simply ignore delay edges, this will make the algorithm fail only when
    -- bad cycles are detected
    edges' (TDelay {}) = []
    edges' x           = edges x

--------------------------------------------------------------------------------

-- | ...
cycle :: Unique -> State (Map Unique (Status, Predecessor, Node e)) Bool
cycle u =
  do mark u Visiting
     ns <- adjacent u
     bs <- forM ns $ \n ->
       do p <- predecessor n
          s <- status      n
          case s of
            Unvisited         -> pred n u >> cycle n 
            Visiting | p /= u -> return False
            _                 -> return True
     mark u Visited
     return $ and bs

--------------------------------------------------------------------------------
    
-- | Checks if there are cycles in the given graph, returns true if there are
cycles :: Unique -> [(Unique, Node e)] -> Bool
cycles root nodes = go root init
  where
    init   = M.fromList $ map (fmap ((,,) Unvisited 0)) nodes
    go u s =
      let (b, m) = runState (cycle u) s
          n      = M.filter (\(s, _, _) -> s == Unvisited) m
      in case M.null n of
           True  -> not b
           False -> go (fst $ M.findMin n) m
