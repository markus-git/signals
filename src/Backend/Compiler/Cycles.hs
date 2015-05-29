{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Compiler.Cycles (
    cycles
  )
where

import Frontend.Signal hiding (lift)
  
import Control.Monad.State
import Control.Monad.Writer
import Data.Ref
import Data.Unique

import           Data.Map   (Map, (!))
import qualified Data.Map as M

import Prelude hiding (Left, Right)

--------------------------------------------------------------------------------
-- * 
--------------------------------------------------------------------------------

-- | A node can have three different states during cycle checking
--   * Visited,   no cycles detected in node or children
--   * Visiting,  node is being checked for cycles
--   * Unvisited, node has not yet been checked for cycles
data Status = Visited | Visiting | Unvisited deriving Eq

-- | A node's predecessor
type Predecessor = Unique

-- | ...
data Record   = forall i a. Record (Symbol i a)

-- | ...
type Mapping  = Map Unique (Status, Predecessor)

-- | ...
type M = WriterT [Record] (State Mapping)

--------------------------------------------------------------------------------
-- **

-- | Sets the status of a node
is :: Unique -> Status -> M ()
is u s = modify $ flip M.adjust u $ \(_, p) -> (s, p)

-- | Sets the initial status of a node
as :: Unique -> Status -> M ()
as u s = modify $ M.insert u (s, undefined)

-- | Gets the status of a node
status :: Unique -> M Status
status u = gets $ fst . (! u)

-- | Sets the predecessor of a node
before :: Unique -> Predecessor -> M ()
before u p = modify $ flip M.adjust u $ \(s, _) -> (s, p)

-- | Gets the predecessor of a node
predecessor :: Unique -> M Predecessor
predecessor u = gets $ snd . (! u)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | Checks if the given signal contains cycles
cycles :: Sig i a -> Bool
cycles (Sig (Signal sym)) = flip evalState M.empty $ go sym
  where
    go :: Symbol i a -> State Mapping Bool
    go sym = do
      (b, w) <- runWriterT $ cycle' sym
      bs     <- forM w $ \(Record s) -> go s
      return $ and (b : bs)

--------------------------------------------------------------------------------

cycle' :: Symbol i a -> M Bool
cycle' (Symbol (Ref u sym)) = do
    u `as` Visiting
    b  <- case sym of
       (Repeat  _) -> return False
       (Map   _ s) -> check s
       (Join  l r) -> (&&) <$> check l <*> check r
       (Left    s) -> check s
       (Right   s) -> check s
       (Delay _ s) -> tell [Record s] >> return False
    u `is` Visited
    return b
  where
    check :: Symbol i a -> M Bool
    check sym@(Symbol (Ref n _)) = do
      p <- predecessor n
      s <- status n
      case s of
        Unvisited         -> n `before` u >> cycle' sym
        Visiting | p /= n -> return False
        _                 -> return True

--------------------------------------------------------------------------------
