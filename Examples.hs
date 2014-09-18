module Examples where

import Expr
import Core

import Backend.C

import Frontend.Signal (Signal)
import qualified Frontend.Signal     as S
import qualified Frontend.SignalComp as SC (runSignal)

import Control.Monad.Operational
import Text.PrettyPrint.Mainland

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

ex1 :: Program (CMD Expr) ()
ex1 = do
  ptr <- open "test"
  a   <- fget ptr
  b   <- fget ptr
  fput ptr (a + b + a)

ex2 :: IO (Program (CMD Expr) ())
ex2 = do
  prg <- SC.runSignal sf
  return $ do
     ptr <- open "test"
     let getty = prg (fget ptr)
     let setty = fput ptr
     v  <- getty
     v' <- getty
     setty v
     setty v'
  where
    sf :: Signal (Expr Float) -> Signal (Expr Float)
    sf = S.map (+1)

ex3 :: IO (Program (CMD Expr) ())
ex3 = do
  prg <- SC.runSignal (fir [1.1, 1.2, 1.3])
  return $ do
    ptr <- open "test"
    let getty = prg (fget ptr)
        setty = fput ptr
    v <- getty
    setty v
  where
    fir :: [Expr Float] -> Signal (Expr Float) -> Signal (Expr Float)
    fir as sig = sums $ delays ds sig
        --sums . muls as . delays ds
      where ds = Prelude.replicate (Prelude.length as) 0

    sums :: [Signal (Expr Float)] -> Signal (Expr Float)
    sums = Prelude.foldr1 (+)

    muls :: [Expr Float] -> [Signal (Expr Float)] -> [Signal (Expr Float)]
    muls = Prelude.zipWith (\c s -> S.repeat c * s)

    delays :: [Expr Float] -> Signal (Expr Float) -> [Signal (Expr Float)]
    delays as s = Prelude.tail $ Prelude.scanl (Prelude.flip S.delay) s as

--------------------------------------------------------------------------------
-- **

test = ex3 >>= cgen . mkFunction "test"
