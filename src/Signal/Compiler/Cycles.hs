{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler.Cycles (cycles) where

import Signal.Core
import Signal.Core.Reify

import Control.Monad.State
import Control.Monad.Writer
import Data.Ref
import Data.Ref.Map (Map, Name)
import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right, pred)

import System.Mem.StableName (eqStableName, hashStableName) -- !

--------------------------------------------------------------------------------
-- * 
--------------------------------------------------------------------------------

-- | ...
data Hide f where
  Hide :: f a -> Hide f

--------------------------------------------------------------------------------

-- | A node can have three different states during cycle checking
--   * Visited,   no cycles detected in node or children
--   * Visiting,  node is being checked for cycles
--   * Unvisited, node has not yet been checked for cycles
data Status      = Visited | Visiting | Unvisited deriving Eq

data Tagged i a  = Tagged Status Predecessor (Node i a)

-- | A node's predecessor
data Predecessor = Predecessor (Hide Name) | None

-- | Cycle-checking monad
type M i         = WriterT [Hide (Key i)] (State (Map (Tagged i)))

--------------------------------------------------------------------------------
-- **

untag :: Tagged i a -> Node i a
untag (Tagged _ _ n) = n

(=/=) :: Predecessor -> Predecessor -> Bool
(=/=) (Predecessor (Hide n1)) (Predecessor (Hide n2)) = n1 `eqStableName` n2
(=/=) _ _ = False

-- | Sets the status of a tagged node
is :: Name a -> Status -> M i ()
is r s = modify $ flip M.adjust r $ \(Tagged _ p n) -> Tagged s p n

-- | Sets the predecessor of a tagged node
before :: Name a -> Predecessor -> M i ()
before r p = modify $ flip M.adjust r $ \(Tagged s _ n) -> Tagged s p n

-- | Gets the status of a tagged node
status :: Name a -> M i Status
status r = do
  s <- get
  return $ case M.lookup r s of
    Nothing             -> error $ "Sorter.status: lookup failed"
                                ++ "\n\t i: " ++ show (hashStableName r)
    Just (Tagged s _ _) -> s

-- | Gets the predecessor of a tagged node
predecessor :: Name a -> M i Predecessor
predecessor r =
  do s <- get
     return $ case M.lookup r s of
       Nothing             -> error "Sorter.predecessor: lookup failed"
       Just (Tagged _ p _) -> p

node :: Name (S Symbol i a) -> M i (S Key i a)
node r =
  do s <- get
     return $ case M.lookup r s of
       Nothing                    -> error "Sorter.node: lookup failed"
       Just (Tagged _ _ (Node n)) -> n

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- ! Remove unsafe, It's not really needed.
cycle' :: Key i a -> M i Bool
cycle' key@(Key r) =
  do r `is` Visiting
     n  <- node r
     b  <- case n of
       (Var     _) -> return False
       (Repeat  _) -> return False
       (Map   _ s) -> check s
       (Join  l r) -> (&&) <$> check l <*> check r
       (Left    l) -> check l
       (Right   r) -> check r
       (Delay _ s) -> tell [Hide s] >> return False
     r `is` Visited
     return b
  where
    check :: Key i a -> M i Bool
    check key@(Key r') =
      do let q = Predecessor (Hide r)
         p <- predecessor r'
         s <- status      r'
         case s of
           Unvisited          -> r' `before` q >> cycle' key
           Visiting | p =/= q -> return False
           _                  -> return True
         
--------------------------------------------------------------------------------

-- | Checks if the given signal contains cycles
cycles :: Key i a -> Nodes i -> Bool
cycles key nodes = flip evalState (init nodes) $ go key
  where
    init :: Nodes i -> Map (Tagged i)
    init = M.hmap $ \node -> Tagged Unvisited None node
    
    go :: Key i a -> State (Map (Tagged i)) Bool
    go node =
      do (b, w) <- runWriterT $ cycle' node
         (bs)   <- mapM add w
         return $  and (b : bs)
      where
        add :: Hide (Key i) -> State (Map (Tagged i)) Bool
        add (Hide key@(Key n)) =
          do s <- get
             case M.lookup n s of
               Nothing -> error "Cycles.cyles.add: lookup failed"
               Just (Tagged _ _ (Node (Delay _ k))) ->
                 go k

--------------------------------------------------------------------------------

