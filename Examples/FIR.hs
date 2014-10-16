module FIR where

import Expr
import Core

import Backend.C

import Frontend.Signal (Signal)
import qualified Frontend.Signal     as S
import qualified Frontend.SignalComp as SC (runSignal)

import Control.Monad.Operational
import Text.PrettyPrint.Mainland

--------------------------------------------------------------------------------
-- * FIR Filter Example
--------------------------------------------------------------------------------

fir :: [Expr Float] -> Signal (Expr Float) -> Signal (Expr Float)
fir as = sums . muls as . delays ds
  where ds = Prelude.replicate (Prelude.length as) 0

sums :: [Signal (Expr Float)] -> Signal (Expr Float)
sums = Prelude.foldr1 (+)

muls :: [Expr Float] -> [Signal (Expr Float)] -> [Signal (Expr Float)]
muls as = Prelude.zipWith (*) (Prelude.map S.repeat as)

delays :: [Expr Float] -> Signal (Expr Float) -> [Signal (Expr Float)]
delays as s = Prelude.tail $ Prelude.scanl (Prelude.flip S.delay) s as

--------------------------------------------------------------------------------

ex_fir :: IO (Program (CMD Expr) ())
ex_fir = do
  prg <- SC.runSignal (fir [1.1, 1.2, 1.3])
  return $ do
    ptr <- open "test"
    let getty = prg (fget ptr)
        setty = fput ptr
    v <- getty
    setty v
    close ptr

--------------------------------------------------------------------------------

test = ex_fir >>= cgen . mkFunction "test"
