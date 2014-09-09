module Examples where

import Control.Monad.Operational
import Expr
import Core
import Backend.C
import Text.PrettyPrint.Mainland

ex1 :: Program (CMD Expr) ()
ex1 = do
  ptr <- open "test"
  a   <- fget ptr
  b   <- fget ptr
  fput ptr (a + b + a)
