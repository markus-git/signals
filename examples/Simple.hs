{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Simple where

import Core
import Frontend.Signal hiding (S)
import Frontend.Signal.Observ

--------------------------------------------------------------------------------
-- * Exp
--------------------------------------------------------------------------------

data Exp a
  where
    Val :: Int -> Exp a
    Var :: Show a => a -> Exp a

    Neg :: Exp a -> Exp a
    Add :: Exp a -> Exp a -> Exp a

type Instr = RefCMD Exp

type S     = Sig Instr

--------------------------------------------------------------------------------
-- **

int :: Int -> S Int
int i = lift0 $ Val i

neg :: S Int -> S Int
neg = lift1 Neg

add :: S Int -> S Int -> S Int
add = lift2 Add

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

test_sig :: S Int
test_sig =
  let x = int 1
      y = int 2
   in add (neg x) (neg (add y y))

test =
  let (Sig (Signal sym)) = test_sig
      (k, nodes)         = bepa sym
   in nodes `seq` putStrLn "Hi!"

--------------------------------------------------------------------------------
