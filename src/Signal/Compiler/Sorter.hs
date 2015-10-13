{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler.Sorter (
    Ordered(..)
  , sorter
  )
where

import Signal.Core hiding (lift)
import Signal.Core.Reify
import Signal.Core.Witness
  
import Control.Arrow
import Control.Monad.State
import Data.List (sortBy)
import Data.Function (on)
import Data.Typeable (Typeable)
import Data.Ref
import Data.Ref.Map (Map, Name)
import qualified Data.Ref.Map as M

import System.Mem.StableName (eqStableName)
import Unsafe.Coerce -- !

import Prelude hiding (Left, Right)

--------------------------------------------------------------------------------
-- * Sorting constructs
--------------------------------------------------------------------------------

-- | During the sorting process a node can either be sorted or unvisited 
data Status = Visited | Unvisited deriving Eq

-- | The ordering assigned to a node after being sorted
type Order = Int

-- | Nodes tagged with extra bookeeping labels
data Tagged i a where
  Tagged :: Witness i a
         => Status
         -> Order
         -> Name     (S Symbol i a)
         -> Node   i (S Symbol i a)
         -> Tagged i (S Symbol i a)

--------------------------------------------------------------------------------
-- ** Sorting helpers

-- | Monad used during sorting
type M i = State (Order, Map (Tagged i))

-- | Returns a new and unique ordering
new :: M i Order
new = do
  (i, m) <- get
  put (i + 1, m)
  return i

-- | Updates the order and name-tag of a node
tag :: forall i a. Name (S Symbol i a) -> Order -> M i ()
tag r o = modify $ second $ M.adjust update r
  where
    update :: Tagged i (S Symbol i a) -> Tagged i (S Symbol i a)
    update (Tagged s _ _ n) = Tagged s o r n

-- | Marks a node as visited
visited :: Name (S Symbol i a) -> M i ()
visited r = modify $ second $ M.adjust update r
  where
    update :: Tagged i (S Symbol i a) -> Tagged i (S Symbol i a)
    update (Tagged _ o k n) = Tagged Visited o k n

-- | Gets the status of a node
status :: Name (S Symbol i a) -> M i Status
status r = gets $ (\(Tagged s _ _ _) -> s) . (M.! r) . snd

-- | Gets the node's constructor typ
node :: Name (S Symbol i a) -> M i (S Key i a)
node r = gets $ (\(Tagged _ _ k (Node n)) -> n) . (M.! r) . snd

--------------------------------------------------------------------------------
-- * Sorter
--------------------------------------------------------------------------------

-- | Sorting of individual nodes: mark as visited, follow children, tag with order.
sort' :: Name (S Symbol i a) -> M i ()
sort' r =
  do visited r
     n <- node r
     case n of
       (Var     _) -> return ()
       (Repeat  _) -> return ()
       (Map   _ s) -> visit s
       (Join  l r) -> visit l >> visit r
       (Left    l) -> visit l
       (Right   r) -> visit r
       (Delay _ s) -> visit s
       (Mux   s c) -> visit s >> mapM_ (visit . snd) c
     o <- new
     r `tag` o
  where
    visit :: Key i a -> M i ()
    visit (Key k) =
      do s <- status k
         when (s /= Visited) (sort' k)

--------------------------------------------------------------------------------

-- | Ordered keys with a witness constraint for well-formedness
data Ordered i
  where
    Ordered :: Witness i a => Key i a -> Ordered i

-- | Comparing ordered keys is the same as comparing the keys
instance Eq (Ordered i) where
  Ordered (Key l) == Ordered (Key r) = l `eqStableName` r

-- | Given a root and a set of graph nodes, a topological ordering is produced
sorter :: Key i a -> Nodes i -> [Ordered i]
sorter (Key n) nodes = reduce $ snd $ flip execState (1, init nodes) $ sort' n
  where
    init :: Nodes i -> Map (Tagged i)
    init = M.hmap repack
      where
        repack :: forall i a. Node i a -> Tagged i a
        repack node@(Node (keys :: S Key i b)) = Tagged Unvisited 0 undefined node

    reduce :: Map (Tagged i) -> [Ordered i]
    reduce = fmap snd . sortBy (compare `on` fst) . fmap repack . M.elems
      where
        -- Haskell is strange sometimes...
        repack :: forall i. M.Entry (Tagged i) -> (Order, Ordered i)
        repack (M.Entry name (Tagged _ o k (Node _))) = (o, Ordered (Key k))

--------------------------------------------------------------------------------
