{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Compiler.Linker (
    Resolution(..)
  , TEx
  , linker
  )
where

import Core

import Frontend.Stream (Stream)
import Frontend.Signal (Signal)
import Frontend.Signal.Observ

import Backend.Nested
import Backend.Knot
import Backend.Ex

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Map (Map, (!))
import Data.Reify
import Data.Typeable

import qualified Data.Map as M

--------------------------------------------------------------------------------
-- * Linking
--------------------------------------------------------------------------------

-- | Untyped binary tree over reference names
type TEx instr = Ex (Suple instr)

-- | ... I assume each index yeilds a tree with the expected type
data Resolution symbol instr = Resolution
  { _output :: Map symbol (TEx instr)
  , _input  :: Map symbol (TEx instr)
  }

-- | Constraints over symbol to input/output tree
data Constraint symbol instr
  = In  (symbol, TEx instr)
  | Out (symbol, TEx instr)

--------------------------------------------------------------------------------

-- | Attempts to fetch resolved value of index
resolve :: (MonadReader (Resolution i instr) m, Ord i, Typeable a) => i -> Suple instr a -> m (Suple instr a)
resolve i _ =
  do ex <- asks ((! i) . _output)
     return $ case ex of
       Ex t -> case gcast t of
         Nothing -> error "resolve: type error"
         Just o  -> o

-- | Mark tree with index
mark :: String -> Suple instr a -> Suple instr a
mark s (Seaf   _)   = Seaf s
mark s (Sranch l r) = Sranch (mark (s ++ "_l") l) (mark (s ++ "_r") r)

-- | Adds an output constraint
constrain :: (MonadWriter [Constraint i instr] m, Typeable a) => i -> Suple instr a -> m ()
constrain i t = tell [Out (i, Ex t)]

-- | Adds an input constraint
introduce :: (MonadWriter [Constraint i instr] m, Typeable a) => i -> Suple instr a -> m ()
introduce i t = tell [In  (i, Ex t)]

--------------------------------------------------------------------------------

-- | Given a signal node, link creates constraints modeling its relation to others
link :: forall m i instr. (Monad m, Ord i, Show i, Typeable instr)
     => (i, TSignal instr i)
     -> Knot (Resolution i instr)
             (Constraint i instr) m
             ()

link (i, TLambda l r) =
  do return ()

link (i, TVar t) =
  do constrain i $ mark (show i) t

link (i, TConst (c :: Stream instr (IExp instr a))) =
  do constrain i (Seaf (show i) :: Suple instr (Empty instr a)) 

link (i, TLift (f :: Stream instr (IExp instr a) -> Stream instr (IExp instr b)) s) =
  do let t = undefined :: Suple instr (Empty instr a)
     t' <- resolve s t
     introduce i t'
     constrain i (Seaf (show i) :: Suple instr (Empty instr a))

link (i, TDelay (e :: IExp instr a) s) =
  do let t = undefined :: Suple instr (Empty instr a)
     t' <- resolve s t
     introduce i t'
     constrain i (Seaf (show i) :: Suple instr (Empty instr a))

link (i, TMap ti to f s) =
  do t' <- resolve s ti
     introduce i t'
     constrain i $ mark (show i) to

link (i, TJoin tl tr l r) =
  do tl' <- resolve l tl
     tr' <- resolve r tr
     constrain i $ Sranch tl' tr'

link (i, TLeft t l) =
  do t' <- resolve l t
     constrain i $ lefty t'
  where
    lefty :: Suple instr (a, b) -> Suple instr a
    lefty t = case t of (Sranch l r) -> l

link (i, TRight t r) =
  do t' <- resolve r t
     constrain i $ righty t
  where
    righty :: Suple instr (a, b) -> Suple instr b
    righty t = case t of (Sranch l r) -> r


--------------------------------------------------------------------------------

-- | ...
linker :: Typeable instr => [(Unique, Node instr)] -> Resolution Unique instr
linker = snd . runIdentity . tie solve . sequence . fmap link

-- | ...
solve :: Solver (Resolution Unique instr) (Constraint Unique instr)
solve constraints =
  let inputs  = [ i | In  i <- constraints]
      outputs = [ o | Out o <- constraints]
  in  Resolution
       { _output = M.fromList outputs
       , _input  = M.fromList inputs
       }
