{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Compiler.Sorter (
    Order
  , Ordered(..)
  , sorter
  )
where

import Frontend.Signal hiding (lift)
import Frontend.Signal.Observ
  
import Control.Arrow
import Control.Monad.State
import Data.List (sortBy)
import Data.Function (on)
import Data.Ref
import Data.Ref.Map (Map, Name)
import qualified Data.Ref.Map as M

import Unsafe.Coerce -- !

import Prelude hiding (Left, Right)

--------------------------------------------------------------------------------
-- * Sorter
--------------------------------------------------------------------------------

-- | During the sorting process a node can either be sorted or unvisited 
data Status     = Visited | Unvisited deriving Eq

-- | ...
data Tagged i a = Tagged Status Order (Name a) (Node i a)

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

-- | Updates the order of a node
tag :: Name a -> Order -> M i ()
tag r o = modify $ second $ flip M.adjust r $ \(Tagged s _ k n) -> Tagged s o k n

-- | Marks a node as visited
visited :: Name a -> M i ()
visited r = modify $ second $ flip M.adjust r $ \(Tagged _ o k n) -> Tagged Visited o k n

-- | Gets the status of a node
status :: Name a -> M i Status
status r = gets $ (\(Tagged s _ _ _) -> s) . (M.! r) . snd

-- | ...
node :: Name (S Symbol i a) -> M i (S Key i a)
node r =
  do modify $ second $ flip M.adjust r $ \(Tagged s o _ n) -> Tagged s o r n
     gets   $ (\(Tagged _ _ k (Node n)) -> n) . (M.! r) . snd

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

data Ordered i
  where
    Ordered :: Name (S Symbol i a) -> Ordered i

-- | Given a root and a set of graph nodes, a topological ordering is produced
sorter :: Key i a -> Map (Node i) -> [Ordered i]
sorter (Key n) nodes = reduce $ snd $ flip execState (1, init nodes) $ sort' n
  where
    init :: Map (Node i) -> Map (Tagged i)
    init = M.hmap $ \node -> Tagged Unvisited 0 undefined node

    reduce :: Map (Tagged i) -> [Ordered i]
    reduce = fmap snd
           . sortBy (compare `on` fst)
           . fmap apa
           . fmap snd
           . concat
           . M.dump
      where
        -- ! I'm pretty sure this is ok, look at 'node' function
        apa :: forall i. M.HideType (Tagged i) -> (Order, Ordered i)
        apa (M.Hide (Tagged _ o k _)) = (o, Ordered $ unsafeCoerce k)

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
