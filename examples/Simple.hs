{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simple where

import Signal hiding (Sig, Stream)
import Signal.Compiler
import qualified Signal.Compiler.Backend.VHDL as HDL hiding (compile)
import qualified Signal.Compiler.Backend.C as C hiding (compile)
import qualified Signal.Core as S
import qualified Signal.Core.Stream as Str
import qualified Signal.Core.Frontend as S

import Language.Embedded.Hardware hiding (Sig, Signal, Comp)
import Language.Embedded.Hardware.Expression.Represent
import qualified Language.Embedded.Hardware.Command as HDL
import qualified Language.Embedded.Hardware.Command.CMD as HDL
import qualified Language.Embedded.Hardware.Command.Frontend as HDL
import qualified Language.Embedded.Hardware.Interface as HDL

import qualified Language.Embedded.Expression as C
import qualified Language.Embedded.Imperative as C
import Language.Embedded.CExp
import Language.Embedded.Backend.C
import qualified Language.Embedded.Backend.C as C

import Control.Monad.Identity
import Control.Monad.Operational.Higher

import Data.ALaCarte
import Data.Constraint

import Prelude hiding (repeat, zipWith)

--------------------------------------------------------------------------------
-- * Edge detector.
--------------------------------------------------------------------------------

type HInstr = SignalCMD :+: VariableCMD :+: ConditionalCMD :+: ComponentCMD :+: ProcessCMD :+: VHDLCMD
type HProg  = Program  HInstr (Param2 HExp HType)
type HComp  = HDL.Comp HInstr HExp HType Identity
type HSig   = HDL.Sig  HInstr HExp HType Identity
type Signal = HDL.Signal

--------------------------------------------------------------------------------

instance HType a => Tuple HType (Identity a)
  where
    witness = Single :: TupleRep HType (Identity a)

instance S.Literal HExp HType
  where
    lit Dict a = HDL.litE a

--------------------------------------------------------------------------------

vhdl :: IO ()
vhdl =
  do sig <- HDL.compileF1 ex :: IO (HSig (Signal Bool -> Signal Bool -> ()))
     putStrLn $ HDL.compileSig sig
  where
    ex :: S.Sig HExp HType Bool -> S.Sig HExp HType Bool
    ex sig = zipWith (neq) sig (delay False sig)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

type CInstr = C.ControlCMD :+: C.RefCMD :+: C.ArrCMD
type CProg  = Program CInstr (Param2 CExp CType)
type Stream a = Str.Str CInstr CExp CType a

--------------------------------------------------------------------------------

instance CType a => Tuple CType (Identity a)
  where
    witness = Single :: TupleRep CType (Identity a)

instance S.Literal CExp CType
  where
    lit Dict a = C.constExp a

--------------------------------------------------------------------------------

c :: IO ()
c =
  do prog <- C.compileF1 ex :: IO (Stream Bool -> Stream Bool)
     putStrLn $ C.compile $
       Str.run $ prog $ Str.repeat $ C.constExp False
  where
    ex :: S.Sig CExp CType Bool -> S.Sig CExp CType Bool
    ex sig = zipWith (#!=) sig (delay False sig)

--------------------------------------------------------------------------------
