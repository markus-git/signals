{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}

module Expr where

import Data.Typeable (Typeable)

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
    Sin :: Floating a   => Expr a           -> Expr a
    Mod :: Integral a   => Expr a -> Expr a -> Expr a

    -- ^ Bool. operations
    Not :: Expr Bool              -> Expr Bool
    And :: Expr Bool -> Expr Bool -> Expr Bool
    Or  :: Expr Bool -> Expr Bool -> Expr Bool

    Eq  :: Eq a  => Expr a    -> Expr a -> Expr Bool
    LEq :: Ord a => Expr a    -> Expr a -> Expr Bool
  deriving Typeable

-- | Variable indetifiers
type VarId = String

--------------------------------------------------------------------------------
-- ** Instances

instance (Show a, Eq a) => Eq (Expr a)     -- bad
  where
    a == b = todo
    a /= b = todo

instance (Show a, Ord a) => Ord (Expr a)   -- bad
  where
    a <= b = todo

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

instance (Show a, Num a, Eq a) => Num (Expr a)
  where
    fromInteger   = Val . fromInteger
    Val a + Val b = Val (a+b)
    Val 0 + b     = b
    a     + Val 0 = a
    a     + b     = Add a b
    Val a - Val b = Val (a-b)
    Val 0 - b     = b
    a     - Val 0 = a
    a     - b     = Sub a b
    Val a * Val b = Val (a*b)
    Val 0 * b     = Val 0
    a     * Val 0 = Val 0
    Val 1 * b     = b
    a     * Val 1 = a
    a     * b     = Mul a b

    signum = todo; abs = todo;

instance (Show a, Fractional a, Eq a) => Fractional (Expr a)
  where
    fromRational  = Val . fromRational
    Val a / Val b = Val (a/b)
    a     / b     = Div a b

    recip = todo;

instance (Show a, Floating a, Eq a) => Floating (Expr a)
  where
    pi   = Val pi
    sin  = Sin
    Val a ** Val b = Val (a**b)
    a     ** b     = Exp a b

    exp   = todo; sqrt  = todo; log     = todo;
    tan   = todo; cos   = todo; asin    = todo;
    atan  = todo; acos  = todo; sinh    = todo;
    tanh  = todo; cosh  = todo; asinh   = todo;
    atanh = todo; acosh = todo; logBase = todo;

todo = error "todo in expr" -- I'll add these later

--------------------------------------------------------------------------------
-- **

eq :: Eq a => Expr a -> Expr a -> Expr Bool
eq = Eq

leq :: Ord a => Expr a -> Expr a -> Expr Bool
leq = LEq

lt :: Ord a => Expr a -> Expr a -> Expr Bool
lt l r = (LEq l r) `And` (Not $ Eq r l)

gt :: Ord a => Expr a -> Expr a -> Expr Bool
gt = flip lt

--------------------------------------------------------------------------------
-- * Trees over expressions
--------------------------------------------------------------------------------

{- I don't know where to put these... -}

-- | 0-tuple value
data Empty a
  deriving Typeable

-- | ...
data Struct a
  where
    Leaf :: Typeable a => Expr a -> Struct (Expr a)
    Pair :: Struct a -> Struct b -> Struct (a, b)
  deriving Typeable
