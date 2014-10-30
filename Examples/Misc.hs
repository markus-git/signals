module Examples.Misc where

import Core
import Expr
import Interpretation

import           Frontend.Signal     (Sig)
import qualified Frontend.Signal     as S

import qualified Frontend.SignalObsv as SO
import qualified Frontend.SignalComp as SC

import qualified Backend.C           as B

import Text.PrettyPrint.Mainland

import Control.Monad.Operational (Program)
import Data.Reify
import Data.Typeable

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

ex1 :: Sig Float -> Sig Float
ex1 s = S.map (+1) s

ex2 :: Sig Float -> Sig Float
ex2 s = let (x, _) = f s
            (_, y) = f s
            z      = g (x, y)
         in x + z
  where
    f :: Sig Float -> (Sig Float, Sig Float)
    f = S.lift (\a -> (a + a, a * a))

    g :: (Sig Float, Sig Float) -> Sig Float
    g = S.lift (\(a, b) -> b - a)

--------------------------------------------------------------------------------

genT :: IO Doc
genT = do
  f <- test
  B.cgen $ mkFunction "misc" f

test :: IO (Program (CMD Expr) ())
test = do
  prg <- compile ex1
  return $ do
    ptr <- open "test"
    let getty = fget ptr
        setty = fput ptr

    v <- prg getty
    setty v

    close ptr



----------------------------------------

type Prg a = Program (CMD Expr) (Expr a)

compile :: (Typeable a, Typeable b)
          =>    (Sig a -> Sig b)
          -> IO (Prg a -> Prg b)
compile f = do
  g <- reifyGraph f
  return $ \i -> SC.compileGraph g i
