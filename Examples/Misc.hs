module Examples.Misc where

import Core
import Expr
import Interpretation

import           Frontend.Signal (Sig)
import qualified Frontend.Signal     as S

import qualified Frontend.SignalObsv as SO
import qualified Frontend.SignalComp as SC

import Data.Reify

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

ex1 :: Sig (Expr Int) -> Sig (Expr Int)
ex1 s = S.zipWith (+) s s

ex2 :: Sig (Expr Int) -> Sig (Expr Int)
ex2 s = let (x, _) = f s
            (_, y) = f s
            z      = g (x, y)
         in x + z
  where
    f :: Sig (Expr Int) -> (Sig (Expr Int), Sig (Expr Int))
    f = S.lift (\a -> (a + a, a * a))

    g :: (Sig (Expr Int), Sig (Expr Int)) -> Sig (Expr Int)
    g = S.lift (\(a, b) -> b - a)

--------------------------------------------------------------------------------

test2 = do
  g <- reifyGraph ex2
  case g of
    (Graph nodes root) -> do let m = SC.linkMap nodes root
                             putStrLn $ show m
