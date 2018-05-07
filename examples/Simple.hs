{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simple where

import Signal hiding (Sig)
import Signal.Compiler
import Signal.Compiler.Backend.VHDL
import qualified Signal as Signal

import Language.Embedded.Hardware hiding (Sig, Signal, Comp)
import Language.Embedded.Hardware.Expression.Represent
import qualified Language.Embedded.Hardware.Command as HDL
import qualified Language.Embedded.Hardware.Command.CMD as HDL
import qualified Language.Embedded.Hardware.Command.Frontend as HDL

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

type Prog = Program Instr (Param2 Exp Pred)

type Comp = HDL.Comp Instr Exp Pred Identity

type Signal = HDL.Signal

--------------------------------------------------------------------------------

type Sig a = Signal.Sig Exp Pred a

type Stream a = Str Instr Exp Pred a

--------------------------------------------------------------------------------

instance Pred a => Tuple Pred (Identity a)
  where
    witness = Single :: TupleRep Pred (Identity a)

--------------------------------------------------------------------------------

example :: Sig Bool -> Sig Bool
example sig = zipWith (neq) sig (delay (litE False) sig)

--------------------------------------------------------------------------------

test :: IO ()
test = do
  prog <- compileFun example ::
    IO (Prog (Comp (Signal Bool -> Signal Bool -> ())))
  putStrLn $ HDL.compile $ do
    comp <- prog
    return ()

--------------------------------------------------------------------------------
