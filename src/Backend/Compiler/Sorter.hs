{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Compiler.Sorter (
    Order
  , Ordering
  , sorter
  )
where

import Frontend.Signal hiding (lift)
import Frontend.Signal.Observ
  
import Control.Arrow
import Control.Monad.State
import Data.List (sort)
import Data.Ref
import Data.Ref.Map (Map, Name)

import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right, Ordering)

--------------------------------------------------------------------------------
-- * Sorter
--------------------------------------------------------------------------------

-- | During the sorting process a node can either be sorted or unvisited 
data Status     = Visited | Unvisited deriving Eq

data Tagged i a = Tagged Status Order (Node i a)

-- | The ordering assigned to a node after being sorted
type Order      = Int

-- | ...
type M i = State (Order, Map (Tagged i))

--------------------------------------------------------------------------------
-- **

-- | Returns a new and unique ordering
new :: M i Order
new = do
  (i, m) <- get
  put (i + 1, m)
  return i

--------------------------------------------------------------------------------
-- **

-- | Updates the order of a node
tag :: Name a -> Order -> M i ()
tag r o = modify $ second $ flip M.adjust r $ \(Tagged s _ n) -> Tagged s o n

-- | Marks a node as visited
visited :: Name a -> M i ()
visited r = modify $ second $ flip M.adjust r $ \(Tagged _ o n) -> Tagged Visited o n

-- | Gets the status of a node
status :: Name a -> M i Status
status r = gets $ (\(Tagged s _ _) -> s) . (M.! r) . snd

-- | ...
node :: Name (S Symbol i a) -> M i (S Key i a)
node r = gets $ (\(Tagged _ _ (Node n)) -> n) . (M.! r) . snd

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

data Ordering (i :: (* -> *) -> * -> *) a = Ordering Order

-- | Given a root and a set of graph nodes, a topological ordering is produced
sorter :: Key i a -> Map (Node i) -> Map (Ordering i)
sorter (Key n) nodes = reduce $ snd $ flip execState (0, init nodes) $ sort' n
  where
    init :: Map (Node i) -> Map (Tagged i)
    init = M.hmap $ \node -> Tagged Unvisited 0 node

    reduce :: Map (Tagged i) -> Map (Ordering i)
    reduce = M.hmap $ \(Tagged _ o _) -> Ordering o
    
--------------------------------------------------------------------------------

sort' :: Name (S Symbol i a) -> M i ()
sort' r =
  do visited r
     n <- node r
     case n of
       (Repeat  _) -> return ()
       (Map   _ s) -> visit s
       (Join  l r) -> visit l >> visit r
       (Left    l) -> visit l
       (Right   r) -> visit r
       (Delay _ s) -> visit s
     o <- new
     r `tag` o
  where
    visit :: Key i a -> M i ()
    visit (Key k) =
      do s <- status k
         when (s /= Visited) (sort' k)

--------------------------------------------------------------------------------
