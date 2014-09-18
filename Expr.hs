{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Expr where

import Interpretation

import Data.Typeable (Typeable1)

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
    Mod :: Integral a   => Expr a -> Expr a -> Expr a

    -- ^ Equality
    Eq  :: Eq a  => Expr a -> Expr a -> Expr Bool
    NEq :: Eq a  => Expr a -> Expr a -> Expr Bool
    LEq :: Ord a => Expr a -> Expr a -> Expr Bool

    -- ^ ToDo: Remove
    TupE :: Expr a -> Expr b -> Expr (a, b)
    FstE :: Expr (a, b) -> Expr a
    SndE :: Expr (a, b) -> Expr b

deriving instance Typeable1 Expr

--------------------------------------------------------------------------------
-- **

-- Tuples
tupE = TupE; fstE = FstE; sndE = SndE;

-- Equality
eq, neq :: Eq a => Expr a -> Expr a -> Expr Bool
eq = Eq; neq = NEq

leq :: Ord a => Expr a -> Expr a -> Expr Bool
leq = LEq;

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
evalExpr' (Mod a b) = evalExpr' a `mod` evalExpr' b

--------------------------------------------------------------------------------
-- ** Haskell Instances

instance (Show a, Eq a) => Eq (Expr a)     -- bad
  where
    a == b = evalExp $ Eq  a b
    a /= b = evalExp $ NEq a b

instance (Show a, Ord a) => Ord (Expr a)   -- bad
  where
    a <= b = evalExp $ LEq a b

instance (Show a, Real a) => Real (Expr a) -- bad
  where
    toRational = todo

instance (Show a, Enum a) => Enum (Expr a) -- bad
  where
    toEnum = todo; fromEnum = todo;

instance (Show a, Integral a) => Integral (Expr a)
  where
    mod = Mod

    quotRem = todo; toInteger = todo;

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

    exp   = todo; sqrt  = todo; log     = todo;
    tan   = todo; cos   = todo; asin    = todo;
    atan  = todo; acos  = todo; sinh    = todo;
    tanh  = todo; cosh  = todo; asinh   = todo;
    atanh = todo; acosh = todo; logBase = todo;

todo = error "todo in expr" -- I'll add these later
