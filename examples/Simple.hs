{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simple where

import Signal

import Language.Embedded.Hardware hiding (Sig, Signal)
import Language.Embedded.Hardware.Expression.Represent

import Control.Monad.Identity
import Control.Monad.Operational.Higher

import Data.ALaCarte

import Prelude hiding (repeat, zipWith)

--------------------------------------------------------------------------------
-- A simple example used for testing the compilers.
--------------------------------------------------------------------------------

type Instr = SignalCMD
         :+: VariableCMD
         :+: ConditionalCMD
         :+: ComponentCMD
         :+: ProcessCMD
         :+: VHDLCMD

type Exp = HExp

type Pred = HType

--------------------------------------------------------------------------------

type Signal a = Sig Exp Pred a

type Stream a = Str Instr Exp Pred a

--------------------------------------------------------------------------------

instance Pred a => Tuple Pred (Identity a)
  where
    witness = Single :: TupleRep Pred (Identity a)

--------------------------------------------------------------------------------

example :: Signal Bool -> Signal Bool
example sig = zipWith (neq) sig (delay (litE False) sig)

--------------------------------------------------------------------------------
