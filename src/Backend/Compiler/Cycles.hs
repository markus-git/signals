{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Compiler.Cycles (
    cycles
  )
where

import Frontend.Signal 
import Frontend.Signal.Observ

import Control.Monad.State
import Control.Monad.Writer
import Data.Ref
import Data.Ref.Map (Map, Name)

import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right, pred)

-- Temp !
import System.Mem.StableName (eqStableName)
import Unsafe.Coerce
-- !

--------------------------------------------------------------------------------
-- * 
--------------------------------------------------------------------------------

-- | A node can have three different states during cycle checking
--   * Visited,   no cycles detected in node or children
--   * Visiting,  node is being checked for cycles
--   * Unvisited, node has not yet been checked for cycles
data Status      = Visited | Visiting | Unvisited deriving Eq

data Tagged i a  = Tagged Status (Predecessor i) (Node i a)

data Record i    = Record (forall a. Key i a)

-- | A node's predecessor
data Predecessor i = Predecessor (forall a. Name (S Symbol i a))
                   | None

-- | Cycle-checking monad
type M i         = WriterT [Record i] (State (Map (Tagged i)))

--------------------------------------------------------------------------------

key  :: Record i -> Name (S Symbol i a)
key  (Record (Key n)) = n

untag :: Tagged i a -> Node i a
untag (Tagged _ _ n) = n

untag' :: Tagged i (S Symbol i a) -> S Key i a
untag' (Tagged _ _ (Node n)) = n

(=/=) :: (Predecessor i) -> (Predecessor i) -> Bool
(=/=) (Predecessor n1) (Predecessor n2) = n1 `eqStableName` n2
(=/=) _                _                = False

--------------------------------------------------------------------------------
-- **

-- | Sets the status of a tagged node
is :: Name a -> Status -> M i ()
is r s = modify $ flip M.adjust r $ \(Tagged _ p n) -> Tagged s p n

-- | Gets the status of a tagged node
status :: Name a -> M i Status
status r = gets $ (\(Tagged s _ _) -> s) . (M.! r)

-- | Sets the predecessor of a tagged node
before :: Name a -> Predecessor i -> M i ()
before r p = modify $ flip M.adjust r $ \(Tagged s _ n) -> Tagged s p n

-- | Gets the predecessor of a tagged node
predecessor :: Name a -> M i (Predecessor i)
predecessor r = gets $ (\(Tagged _ p _) -> p) . (M.! r)

node :: Name (S Symbol i a) -> M i (S Key i a)
node r = gets $ untag' . (M.! r)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | Checks if the given signal contains cycles
cycles :: Key i a -> Map (Node i) -> Bool
cycles (Key key) nodes = flip evalState (init nodes) $ go key
  where
    init :: Map (Node i) -> Map (Tagged i)
    init = M.hmap $ \node -> Tagged Unvisited None node
    
    go :: Name (S Symbol i a) -> State (Map (Tagged i)) Bool
    go node = do
      (b, w) <- runWriterT $ cycle' node
      bs     <- forM w $ \(Record (Key n)) ->
        do (Node (Delay _ (Key k))) <- gets $ untag . (M.! n) -- skip the delay
           go k
      return $ and (b : bs)

--------------------------------------------------------------------------------

cycle' :: Name (S Symbol i a) -> M i Bool
cycle' r =
  do r `is` Visiting
     n  <- node r
     b  <- case n of
       (Repeat  _) -> return False
       (Map   _ s) -> check s
       (Join  l r) -> (&&) <$> check l <*> check r
       (Left    l) -> check l
       (Right   r) -> check r
       (Delay _ s) -> tell [Record $ unsafeCoerce s] >> return False
     r `is` Visited
     return b
  where
    check :: Key i a -> M i Bool
    check (Key r') =
      do let q = Predecessor $ unsafeCoerce r
         p <- predecessor r'
         s <- status      r'
         case s of
           Unvisited          -> r' `before` q >> cycle' r'
           Visiting | p =/= q -> return False
           _                  -> return True
         
--------------------------------------------------------------------------------
