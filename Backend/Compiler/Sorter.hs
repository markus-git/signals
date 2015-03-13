module Backend.Compiler.Sorter (
    Order
  , sorter
  )
where

import Frontend.TSignal (TSignal(..), Node, edges)
import Control.Arrow
import Control.Monad.State
import Data.Reify 
import Data.Map (Map, (!))

import qualified Data.Map as M

--------------------------------------------------------------------------------
-- * Sorter
--------------------------------------------------------------------------------

-- | During the sorting process a node can either be sorted or unvisited 
data Status = Visited | Unvisited

-- | The ordering assigned to a node after being sorted
type Order  = Int

--------------------------------------------------------------------------------

-- | Returns a new and unique ordering
new :: State (Int, m) Order
new = do (i, m) <- get
         put (i + 1, m)
         return i

-- | Updates the order of a node
tag :: Unique -> Order -> State (i, Map Unique (s, Order, n)) ()
tag u o = modify $ second $ flip M.adjust u $ \(s, _, n) -> (s, o, n)

-- | Updates the status of a node
mark :: Unique -> Status -> State (i, Map Unique (Status, o, n)) ()
mark u s = modify $ second $ flip M.adjust u $ \(_, o, n) -> (s, o, n)

-- | Gets the status of a node
status :: Unique -> State (i, Map Unique (Status, o, n)) Status
status u = get >>= return . (\(s, _, _) -> s) . (! u) . snd

-- | Gets the adjacent nodes of an node
adjacent :: Unique -> State (i, Map Unique (s, o, Node e)) [Unique]
adjacent u = get >>= return . edges . (\(_, _, n) -> n) . (! u) . snd

--------------------------------------------------------------------------------

-- | Standard depth-first ordering of a graph
--
-- I wonder if this would look nicer when using knots intsead..
sort :: Unique -> State (Int, Map Unique (Status, Order, Node e)) ()
sort u =
  do mark u Visited
     ns <- adjacent u
     forM_ ns $ \n ->
       do s <- status n
          case s of
            Visited   -> return ()
            Unvisited -> sort n
     o <- new
     tag u o

--------------------------------------------------------------------------------

-- | Given a root and a set of graph nodes, a topological ordering is produced
sorter :: Unique -> [(Unique, Node e)] -> Map Unique Order
sorter root nodes = M.map (\(_, o, _) -> o) $ snd $ execState (sort root) init
  where
    init = (1, M.fromList $ map (fmap ((,,) Unvisited 0)) nodes)
