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
    Not ::          Expr Bool           -> Expr Bool
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

instance (Show a, Num a) => Num (Expr a)
  where
    fromInteger = Val . fromInteger
    (+)         = Add
    (-)         = Sub
    (*)         = Mul

    signum = todo; abs = todo;

instance (Show a, Fractional a) => Fractional (Expr a)
  where
    fromRational = Val . fromRational
    (/)          = Div

    recip = todo;

instance (Show a, Floating a) => Floating (Expr a)
  where
    pi   = Val pi
    sin  = Sin
    (**) = Exp

    exp   = todo; sqrt  = todo; log     = todo;
    tan   = todo; cos   = todo; asin    = todo;
    atan  = todo; acos  = todo; sinh    = todo;
    tanh  = todo; cosh  = todo; asinh   = todo;
    atanh = todo; acosh = todo; logBase = todo;

todo = error "todo in expr" -- I'll add these later

--------------------------------------------------------------------------------
-- * Tree's of expressions
--------------------------------------------------------------------------------

{- I don't know where to put these... -}

-- | 0-tuple value
-- data Empty a
--   deriving Typeable

-- | ...
data Struct a
  where
    Leaf :: Typeable a => Expr a   -> Struct (Expr a)
    Pair :: Struct a   -> Struct b -> Struct (a, b)
  deriving Typeable

--------------------------------------------------------------------------------
-- ** ToDo: Come up with a better name

class Classy a
  where
    varStruct :: VarId -> Struct a

instance (Typeable a) => Classy (Expr a)
  where
    varStruct = Leaf . Var

instance (Classy a, Classy b) => Classy (a, b)
  where
    varStruct i = Pair (varStruct (i ++ "_1"))
                       (varStruct (i ++ "_2"))
