{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Examples.Simple.Expr where

import Core (EEq(..))
import Interpretation

import Backend.C.Monad

import           Language.C.Quote.C
import qualified Language.C.Syntax as C
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
    I2N :: (Integral a, Num b) => Expr a -> Expr b

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

i2n :: (Integral a, Num b) => Expr a -> Expr b
i2n = I2N

todo = error "todo in expr" -- I'll add these later

--------------------------------------------------------------------------------
-- **

instance Eq a => EEq Expr a
  where
    (==:) = eq
    (/=:) = neq

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

tru :: Expr Bool
tru = Val True

fls :: Expr Bool
fls = Val False

eq :: Eq a => Expr a -> Expr a -> Expr Bool
eq = Eq

neq :: Eq a => Expr a -> Expr a -> Expr Bool
neq a b = Not $ a `eq` b

leq :: Ord a => Expr a -> Expr a -> Expr Bool
leq = LEq

lt :: Ord a => Expr a -> Expr a -> Expr Bool
lt l r = (LEq l r) `And` (Not $ Eq r l)

gt :: Ord a => Expr a -> Expr a -> Expr Bool
gt = flip lt

--------------------------------------------------------------------------------
-- * Evaluation
--------------------------------------------------------------------------------

instance EvalExp Expr
  where
    type LitPred Expr = Show

    litExp  = Val
    evalExp = evalExpr'

-- |
evalExpr' :: Expr a -> a
evalExpr' (Val a) = a
evalExpr' (Var _) = error "cannot eval var"

-- ^ Math. ops.
evalExpr' (Add a b) = evalExpr' a + evalExpr' b
evalExpr' (Sub a b) = evalExpr' a - evalExpr' b
evalExpr' (Mul a b) = evalExpr' a * evalExpr' b
evalExpr' (Div a b) = evalExpr' a / evalExpr' b
evalExpr' (Mod a b) = evalExpr' a `mod` evalExpr' b
evalExpr' (Sin a)   = sin $ evalExpr' a
evalExpr' (I2N a)   = fromInteger $ fromIntegral $ evalExpr' a

-- ^ Bool. ops.
evalExpr' (Not   a) = not $ evalExpr' a
evalExpr' (And a b) = evalExpr' a && evalExpr' b
evalExpr' (Or  a b) = evalExpr' a || evalExpr' b
evalExpr' (Eq  a b) = evalExpr' a == evalExpr' b
evalExpr' (LEq a b) = evalExpr' a <= evalExpr' b

--------------------------------------------------------------------------------
-- * Compilation of Expressions
--------------------------------------------------------------------------------

class    Any a
instance Any a

instance CompExp Expr
  where
    type VarPred Expr = Any
    varExp   = Var
    compExp  = compExp'

-- |
compExp' :: Expr a -> C C.Exp
compExp' (Var v) = return [cexp| $id:v |]
compExp' (Val v) = case show v of
    "True"  -> addInclude "<stdbool.h>" >> return [cexp| true |]
    "False" -> addInclude "<stdbool.h>" >> return [cexp| false |]
    v'      -> return [cexp| $id:v' |]

-- ^ Math. ops.
compExp' (Add a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' + $b' |]
compExp' (Sub a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' - $b' |]
compExp' (Mul a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' * $b' |]
compExp' (Div a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' / $b' |]
compExp' (Exp a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' ^ $b' |]
compExp' (Sin a)   = do
  a' <- compExp' a
  return [cexp| sin( $a' ) |]
compExp' (Mod a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' % $b'|]
compExp' (I2N a) = do
  a' <- compExp' a
  return [cexp| $a' |]

-- ^ Bool. ops.
compExp' (Not  a)  = do
  a' <- compExp' a
  return [cexp| ! $a' |]
compExp' (And a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| ($a' && $b') |]
compExp' (Or a b)  = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| ($a' || $b') |]
compExp' (Eq a b)  = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' == $b' |]
compExp' (LEq a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' <= $b' |]


