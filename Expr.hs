{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Expr where

import Interpretation

--------------------------------------------------------------------------------
-- * Expressions
--------------------------------------------------------------------------------

-- |
data Expr a
  where
    Val :: Show a => a -> Expr a
    Var :: VarId       -> Expr a

    -- ^ Math. operations
    Add :: Num a        => Expr a -> Expr a -> Expr a
    Sub :: Num a        => Expr a -> Expr a -> Expr a
    Mul :: Num a        => Expr a -> Expr a -> Expr a
    Div :: Fractional a => Expr a -> Expr a -> Expr a
    Exp :: Floating a   => Expr a -> Expr a -> Expr a
    Sin :: Floating a   => Expr a -> Expr a

    -- ^ ToDo: Remove
    TupE :: Expr a -> Expr b -> Expr (a, b)
    FstE :: Expr (a, b) -> Expr a
    SndE :: Expr (a, b) -> Expr b

--------------------------------------------------------------------------------
-- **

-- Tuples
tupE = TupE; fstE = FstE; sndE = SndE;

--------------------------------------------------------------------------------
-- ** Evaluation

instance EvalExp Expr
  where
    type LitPred Expr = Show

    litExp  = Val
    evalExp = evalExpr'

evalExpr' :: Expr a -> a
evalExpr' (Val a)   = a
evalExpr' (Add a b) = evalExpr' a + evalExpr' b
evalExpr' (Sub a b) = evalExpr' a - evalExpr' b
evalExpr' (Mul a b) = evalExpr' a * evalExpr' b
evalExpr' (Div a b) = evalExpr' a / evalExpr' b
evalExpr' (Sin a)   = sin $ evalExpr' a

--------------------------------------------------------------------------------
-- ** Haskell Instances

instance (Show a, Num a) => Num (Expr a)
  where
    fromInteger = litExp . fromInteger
    (+)         = Add
    (-)         = Sub
    (*)         = Mul

    signum = todo; abs = todo;

instance (Show a, Fractional a) => Fractional (Expr a)
  where
    fromRational = litExp . fromRational
    (/)          = Div

    recip = todo;

instance (Show a, Floating a) => Floating (Expr a)
  where
    pi   = litExp pi
    sin  = Sin
    (**) = Exp

    exp   = todo; sqrt  = todo; log     = todo
    tan   = todo; cos   = todo; asin    = todo
    atan  = todo; acos  = todo; sinh    = todo
    tanh  = todo; cosh  = todo; asinh   = todo
    atanh = todo; acosh = todo; logBase = todo

-- I'll add these later
todo = undefined
