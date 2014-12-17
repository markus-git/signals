{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Interpretation where

import Expr (Expr, VarId)

import Data.Constraint

import Language.C.Syntax (Exp)

--------------------------------------------------------------------------------
-- * Evaluation
--------------------------------------------------------------------------------

-- | General interface for evaluating expressions
class EvalExp exp
  where
    -- | Predicate for literals
    type LitPred exp :: * -> Constraint

    -- | Literal expressions
    litExp  :: LitPred exp a => a -> exp a

    -- | Evaluation of (closed) expressions
    evalExp :: exp a -> a

-------------------------------------------------------------------------------
-- * Compilation
-------------------------------------------------------------------------------

-- | General interface for compiling expressions
class CompExp m exp
  where
    -- | Predicate for variables
    type VarPred exp :: * -> Constraint

    -- | Variable expressions
    varExp  :: VarPred exp a => VarId -> exp a

    -- | Compilation of expressions
    compExp :: exp a -> m Exp

--------------------------------------------------------------------------------
-- **

-- | General interface for compiling constructs
class CompCMD m cmd
  where
    -- | Compilation of constructs
    compCMD :: cmd a -> m a
