{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples.VHDL where

import Core

import           Frontend.Signal (Sig)
import qualified Frontend.Signal as S

import Backend.Compiler.Compiler

import Data.Dynamic
import Data.Typeable

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

class (Typeable a, Show a) => Typ a
instance Typ (Bool)
instance Typ (Int)

data VExpr a
  where
    Val :: Typ a => a     -> VExpr a
    Var :: Typ a => VarId -> VExpr a
    
    Not :: VExpr Bool -> VExpr Bool
    And :: VExpr Bool -> VExpr Bool -> VExpr Bool
    Or  :: VExpr Bool -> VExpr Bool -> VExpr Bool
    XOr :: VExpr Bool -> VExpr Bool -> VExpr Bool

    Gte :: Ord a => VExpr a -> VExpr a -> VExpr Bool
    Eq  :: Eq  a => VExpr a -> VExpr a -> VExpr Bool
    
    Add :: Num a      => VExpr a -> VExpr a -> VExpr a
    Sub :: Num a      => VExpr a -> VExpr a -> VExpr a
    Mul :: Num a      => VExpr a -> VExpr a -> VExpr a
    Div :: Integral a => VExpr a -> VExpr a -> VExpr a
    Mod :: Integral a => VExpr a -> VExpr a -> VExpr a

type V = Sig (RefCMD VExpr)

--------------------------------------------------------------------------------
-- **

type instance VarPred VExpr = Typ

instance Num (VExpr Int)
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

    abs    = error "abs not implemented for Expr"
    signum = error "signum not implemented for Expr"

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

low, high :: V Bool
low  = S.repeat (Val False)
high = S.repeat (Val True)

inv :: V Bool -> V Bool
inv = S.map Not

andl, orl, xorl :: V Bool -> V Bool -> V Bool
andl = S.zipWith And
orl  = S.zipWith Or
xorl = S.zipWith XOr

--------------------------------------------------------------------------------
-- **

eqB :: V Bool -> V Bool -> V Bool
eqB x y = inv $ xorl x y

ifB :: V Bool -> (V Bool, V Bool) -> V Bool
ifB b (x, y) = orl (andl b y) (andl (inv b) y)

--------------------------------------------------------------------------------
-- **

neg :: V Int -> V Int
neg = S.map (Mul (Val (-1)))

divide, modulo :: V Int -> V Int -> V Int
divide = S.zipWith (Div)
modulo = S.zipWith (Mod)

plusl, subl, timesl  :: V Int -> V Int -> V Int
plusl  = (+)
subl   = (-)
timesl = (*)

eqI :: V Int -> V Int -> V Bool
eqI = S.zipWith (Eq)

gteI :: V Int -> V Int -> V Bool
gteI = S.zipWith (Gte)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

instance EvalExp VExpr
  where
    litExp  = Val
    evalExp = eval (const $ error "eval: free variable")

eval :: (VarId -> Dynamic) -> VExpr a -> a
eval env (Val a) = a
eval env (Var v) | Just a <- fromDynamic (env v) = a
eval env (vexpr) = case vexpr of
  (Not a)   -> not (eval env a)
  (And a b) -> (eval env a) && (eval env b)
  (Or  a b) -> (eval env a) || (eval env b)
  (XOr a b) -> (a' || b') && not (a' && b')
    where a' = (eval env a)
          b' = (eval env b)

  (Gte a b) -> (eval env a) > (eval env b)
  (Eq  a b) -> (eval env a) == (eval env b)
    
  (Add a b) -> (eval env a) + (eval env b)
  (Sub a b) -> (eval env a) - (eval env b)
  (Mul a b) -> (eval env a) * (eval env b)
  (Div a b) -> (eval env a) `div` (eval env b)
  (Mod a b) -> (eval env a) `mod` (eval env b)

--------------------------------------------------------------------------------
-- **

