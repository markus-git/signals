module Examples where

import Expr
import Core

import Backend.C

import Frontend.Signal (Signal)
import qualified Frontend.Signal     as S
import qualified Frontend.SignalComp as SC (runSignal)

import Control.Monad.Operational
import Text.PrettyPrint.Mainland

ex1 :: Program (CMD Expr) ()
ex1 = do
  ptr <- open "test"
  a   <- fget ptr
  b   <- fget ptr
  fput ptr (a + b + a)

ex2 :: IO (Program (CMD Expr) ())
ex2 =
  do prg <- SC.runSignal sf
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

----------------------------------------

test = ex2 >>= cgen . mkFunction "test"
