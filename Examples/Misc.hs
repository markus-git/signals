module Examples.Misc where

import Core
import Expr
import Interpretation

import qualified Frontend.Signal     as S
import qualified Frontend.SignalObsv as SO
import qualified Frontend.SignalComp as SC

import Data.Reify

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

ex1 :: S.Sig (Expr Int) -> S.Sig (Expr Int)
ex1 s = S.zipWith (+) s s

ex2 :: S.Sig (Expr Int) -> S.Sig (Expr Int)
ex2 = g . f
  where
    f :: S.Sig (Expr Int) -> S.Sig (Expr Int, Expr Int)
    f = S.lift (\a -> (a + a, a * a))

    g :: S.Sig (Expr Int, Expr Int) -> S.Sig (Expr Int)
    g = S.lift (\(a, b) -> b - a)
