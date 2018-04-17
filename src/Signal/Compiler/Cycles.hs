{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler.Cycles (cycles) where

import Signal.Core (Signal (..), Symbol (..), Core (..))
import Signal.Core.Reify
import Signal.Core.Data

import Control.Monad.State
import Control.Monad.Writer
import Data.Ref
import Data.Ref.Map (Map, Name)
import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right, pred, cycle)

import System.Mem.StableName (eqStableName, hashStableName) -- !

--------------------------------------------------------------------------------
-- * Cycle checker.
--------------------------------------------------------------------------------

-- | A node can have three different states during cycle checking:
--   * Visited, no cycles detected in node or children.
--   * Visiting, node is being checked for cycles.
--   * Unvisited, node has not yet been checked for cycles.
data Status = Visited | Visiting | Unvisited deriving Eq

-- | A node's predecessor.
data Predecessor = Predecessor (Hide Name) | None

-- | A node, tagged with its predecessor and status.
data Tagged exp pred a = Tagged Status Predecessor (Node exp pred a)

-- | Cycle checking monad.
type CycleM exp pred = WriterT [Hide (Key exp pred)] (State (Map (Tagged exp pred)))

--------------------------------------------------------------------------------

-- | Remove the tag from a tagged node.
untag :: Tagged exp pred a -> Node exp pred a
untag (Tagged _ _ n) = n

-- | Check if two predecessors reference the same node.
(=/=) :: Predecessor -> Predecessor -> Bool
(=/=) (Predecessor (Hide n1)) (Predecessor (Hide n2)) = n1 `eqStableName` n2
(=/=) _ _ = False

-- | Sets the status of a tagged node.
is :: Name a -> Status -> CycleM exp pred ()
is r s = modify $ flip M.adjust r $ \(Tagged _ p n) -> Tagged s p n

-- | Sets the predecessor of a tagged node.
before :: Name a -> Predecessor -> CycleM exp pred ()
before r p = modify $ flip M.adjust r $ \(Tagged s _ n) -> Tagged s p n

-- | Gets the status of a tagged node.
status :: Name a -> CycleM exp pred Status
status r =
  do s <- get
     return $ case M.lookup r s of
       Just (Tagged s _ _) -> s
       Nothing -> error $
         "Cycles.status: lookup failed: " ++ show (hashStableName r)

-- | Gets the predecessor of a tagged node
predecessor :: Name a -> CycleM exp pred Predecessor
predecessor r =
  do s <- get
     return $ case M.lookup r s of
       Just (Tagged _ p _) -> p
       Nothing -> error $
         "Cycles.predecessor: lookup failed: " ++ show (hashStableName r)

-- | Gets the node reference by a name.
node :: Name (Core Symbol exp pred a) -> CycleM exp pred (Core Key exp pred a)
node r =
  do s <- get
     return $ case M.lookup r s of
       Just (Tagged _ _ (Node n)) -> n
       Nothing -> error $
         "Cycles.node: lookup failed: " ++ show (hashStableName r)

--------------------------------------------------------------------------------

cycle :: Key exp pred a -> CycleM exp pred Bool
cycle key@(Key r) =
  do r `is` Visiting
     n  <- node r
     b  <- case n of
       (Var _)     -> return False
       (Val _)     -> return False
       (Map _ s)   -> check s
       (Pair l r)  -> (&&) <$> check l <*> check r
       (Fst l)     -> check l
       (Snd r)     -> check r
       (Delay _ s) -> tell [Hide s] >> return False
--       (Mux   s c) -> (&&) <$> check s <*> (and <$> mapM (check . snd) c)
     r `is` Visited
     return b
  where
    check :: Key exp pred a -> CycleM exp pred Bool
    check key@(Key r') =
      do let q = Predecessor (Hide r)
         p <- predecessor r'
         s <- status      r'
         case s of
           Unvisited          -> r' `before` q >> cycle key
           Visiting | p =/= q -> return False
           _                  -> return True
         
--------------------------------------------------------------------------------

-- | Checks if the given signal contains cycles
cycles :: Key exp pred a -> Nodes exp pred -> Bool
cycles key nodes = flip evalState (init nodes) $ go key
  where
    init :: Nodes exp pred -> Map (Tagged exp pred)
    init = M.hmap $ \node -> Tagged Unvisited None node
    
    go :: Key exp pred a -> State (Map (Tagged exp pred)) Bool
    go node =
      do (b, w) <- runWriterT $ cycle node
         (bs)   <- mapM add w
         return $  and (b : bs)
      where
        add :: Hide (Key exp pred) -> State (Map (Tagged exp pred)) Bool
        add (Hide key@(Key n)) =
          do s <- get
             case M.lookup n s of
               Nothing -> error "Cycles.cyles.add: lookup failed"
               Just (Tagged _ _ (Node (Delay _ k))) ->
                 go k

--------------------------------------------------------------------------------

