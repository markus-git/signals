{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Filters where

import Signal
import Signal.Core (Literal(..))
import Signal.Compiler.Backend.VHDL

import qualified Language.Embedded.Hardware.Expression as E
import qualified Language.Embedded.Hardware.Expression.Represent as E
import qualified Language.Embedded.Hardware.Command    as C
import qualified Language.Embedded.Hardware.Command.Frontend as C
import qualified Language.Embedded.Hardware.Interface  as I
import Control.Monad.Operational.Higher
import Control.Monad.Identity
import Data.ALaCarte
import Data.Bits
import Data.Constraint (Dict(..))
import Data.Typeable
import Data.Word
import System.IO
import Prelude hiding (repeat, map, zipWith, div)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Hardware signals.
--------------------------------------------------------------------------------

-- | Short-hand for expressions type.
type Exp  = E.HExp

-- | Short-hand for predicate type.
type Pred = E.HType

-- | Short-hand for signals over above instructions and expressinos.
type S = Sig Exp Pred

instance Literal E.HExp E.HType where
  lit Dict = I.litE

--------------------------------------------------------------------------------
-- ** Simple circuits

-- | Inverter for a signal.
inv :: S Bool -> S Bool
inv = map E.not

-- | Logical and for two signals.
and2 :: S Bool -> S Bool -> S Bool
and2 = zipWith E.and

-- | Recursiv signal to create an alternating signal.
toggle :: S Bool
toggle = True `delay` (inv toggle)

{-
-- | Todo.
multiplexer :: S Word8 -> S Word8
multiplexer sig = mux2 toggle (sig, zeroes)
  where
    zeroes = repeat 0
    toggle = repeat high
-}
--------------------------------------------------------------------------------
-- ** Filters.

-- | Join two signals by division.
divs :: S Word8 -> S Word8 -> S Word8
divs = zipWith E.div

-- | Join a number of signals by addition.
sums :: [S Word8] -> S Word8
sums = P.foldr1 (+)

-- | Multiply a number of signals by some coefficients.
muls :: [Exp Word8] -> [S Word8] -> [S Word8]
muls es = P.zipWith (*) (P.map repeat es)

-- | Create a number of signals by iteratively delaying them by some value.
delays :: Word8 -> S Word8 -> [S Word8]
delays e = P.iterate (delay e)

-- | Finite impulse response filter.
fir :: [Exp Word8] -> S Word8 -> S Word8
fir as = sums . muls as . delays 0

-- | Infinite impulse response filter
iir :: [Exp Word8] -> [Exp Word8] -> S Word8 -> S Word8
iir (a:as) bs s = o
  where
    u = fir bs s
    l = fir as $ delay 0 o
    o = (1 `divs` repeat a) * (u - l)

--------------------------------------------------------------------------------
-- ** Tuples

-- | Tuples used within signal computations.
tupleS :: S Word8 -> S Word8
tupleS s = let (d, m) = divMod s 2 in d + m

{-
-- | Todo
tupleE :: S Word8 -> S (Word8, S Word8)
tupleE s = undefined
  where p = undefined :: proxy i (Identity a, Identity a) (a, a)
-}

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | Short-hand for instruction set.
type Instr = C.SignalCMD :+: C.VariableCMD :+: C.ComponentCMD :+: C.ProcessCMD

-- | Short-hand for programs over our expression and predicate types.
type P = Program Instr (Param2 Exp Pred)

comp :: (E.PrimType a, Integral a) => S a -> IO ()
comp f =
  do sig :: C.Sig Instr Exp Pred Identity (C.Signal a -> ()) <- compile f
     C.icompileSig sig

-- | Compile a signal function into a signature, and then produce a design.
compF1 ::
     ( E.PrimType a, Integral a
     , E.PrimType b, Integral b)
  => (S a -> S b) -> IO ()
compF1 f =
  do sig :: C.Sig Instr Exp Pred Identity (C.Signal a -> C.Signal b -> ()) <- compileF1 f
     C.icompileSig sig

--------------------------------------------------------------------------------

testInv = compF1 inv
testRec = comp   toggle

testFIR = compF1 (fir [1,2])
testIIR = compF1 (iir [1,2] [3,4])

testTupleS = compF1 tupleS
--testTupleE = compF1 tupleE

--------------------------------------------------------------------------------
