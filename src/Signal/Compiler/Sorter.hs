{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler.Sorter
  ( HiddenKey(..)
  , Ordering
  , sorter
  )
  where

import Signal.Core (Core(..), Symbol(..))
import Signal.Core.Data
import Signal.Core.Reify
--import Signal.Core.Witness
  
import Control.Arrow
import Control.Monad.State
import Data.List (sortBy)
import Data.Function (on)
import Data.Typeable (Typeable)
import System.Mem.StableName (eqStableName)

-- observable-sharing
import Data.Ref
import Data.Ref.Map (Map, Name)
import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right, Ordering)

--------------------------------------------------------------------------------
-- * Sorting.
--------------------------------------------------------------------------------

-- | During the sorting process a node can either be sorted or unvisited.
data Status = Visited | Unvisited deriving Eq

-- | The ordering assigned to a node after being sorted.
type Order = Int

-- | Nodes tagged with extra bookeeping labels.
data Tagged exp pred a where
  Tagged :: Status
         -> Order
         -> Node exp pred (Core Symbol exp pred a)
         -> Tagged exp pred (Core Symbol exp pred a)

-- | Monad used during sorting.
type SortM exp pred = State (Int, Map (Tagged exp pred))

--------------------------------------------------------------------------------

-- | Set the current status of a tagged node.
setStatus :: Status -> Tagged exp pred a -> Tagged exp pred a
setStatus s (Tagged _ o n) = Tagged s o n

-- | Get the current status of a tagged node.
getStatus :: Tagged exp pred a -> Status
getStatus (Tagged s _ _) = s

-- | Set the current order of a tagged node.
setOrder :: Order -> Tagged exp pred a -> Tagged exp pred a
setOrder o (Tagged s _ n) = Tagged s o n

--------------------------------------------------------------------------------

-- | Returns a new and unique ordering.
new :: SortM exp pred Order
new =
  do (i, m) <- get
     put (i + 1, m)
     return i

-- | Updates the order and name-tag of a node.
order :: Name (Core Symbol exp pred a) -> Order -> SortM exp pred ()
order name o = modify $ second $ M.adjust (setOrder o) name -- used to set name.

-- | Mark node as visited.
visited :: Name (Core Symbol exp pred a) -> SortM exp pred ()
visited = modify . second . M.adjust (setStatus Visited)

-- | Get status of node.
status :: Name (Core Symbol exp pred a) -> SortM exp pred Status
status name = gets $ getStatus . (M.! name) . snd

-- | Get node's constructor type.
node :: Name (Core Symbol exp pred a) -> SortM exp pred (Core Key exp pred a)
node name = gets $ (\(Tagged _ _ (Node core)) -> core) . (M.! name) . snd

--------------------------------------------------------------------------------

-- | Sorting of individual nodes. General strategy during sorting is to mark a
--   node as visited, sort its children, then tag with order.
sort :: Name (Core Symbol exp pred a) -> SortM exp pred ()
sort name =
  do visited name
     n <- node name
     case n of
       (Var _)     -> return ()
       (Val _)     -> return ()
       (Map _ s)   -> visit s
       (Pair l r)  -> visit l >> visit r
       (Fst l)     -> visit l
       (Snd r)     -> visit r
       (Delay _ s) -> visit s
--       (Mux   s c) -> visit s >> mapM_ (visit . snd) c
     order name =<< new

-- | Sort a node if it hasn't been visited yet.
visit :: Key exp pred a -> SortM exp pred ()
visit (Key k) =
  do s <- status k
     when (s /= Visited) (sort k)

--------------------------------------------------------------------------------

-- | Ordered keys.
newtype HiddenKey exp pred = HiddenKey { reveal :: Hide (Key exp pred) }

-- | Comparing ordered keys is the same as comparing the keys.
instance Eq (HiddenKey exp pred) where
  HiddenKey (Hide (Key l)) == HiddenKey (Hide (Key r)) = l `eqStableName` r

-- | List of ordered keys.
type Ordering exp pred = [HiddenKey exp pred]

-- | Given a root and a set of graph nodes, a topological ordering is produced.
sorter :: Key exp pred a -> Nodes exp pred -> Ordering exp pred
sorter (Key n) nodes = reduce $ snd $ flip execState (1, init nodes) $ sort n
  where
    init :: Nodes exp pred -> Map (Tagged exp pred)
    init = M.hmap repack
      where
        repack :: Node exp pred a -> Tagged exp pred a
        repack node@(Node keys) = Tagged Unvisited 0 node

    reduce :: Map (Tagged exp pred) -> Ordering exp pred
    reduce = fmap snd . sortBy (compare `on` fst) . fmap repack . M.elems
      where
        repack :: M.Entry (Tagged exp pred) -> (Order, HiddenKey exp pred)
        repack (M.Entry name (Tagged _ order _)) =
          (order, HiddenKey (Hide (Key name)))

--------------------------------------------------------------------------------
