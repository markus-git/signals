module Examples.Misc where

{-
-- TODO: these examples rely on Expr, which is no longer present

import Core
import Expr -- ?
import Interpretation

import           Frontend.Signal     (Sig)
import qualified Frontend.Signal     as S

import qualified Frontend.SignalObsv as SO
-- import qualified Frontend.SignalComp as SC

import qualified Backend.C           as B

import Text.PrettyPrint.Mainland

import Control.Monad.Operational (Program)
import Data.Reify
import Data.Typeable

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

ex1 :: Sig Float -> Sig Float
ex1 s = S.map (+1) $ S.map (+2) s

-- doesn't work yet..
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

test :: IO Doc
test = do
  f <- testF
  B.cgen $ mkFunction "misc" f

testF :: IO (Program (CMD Expr) ())
testF = do
  prg <- compile ex1
  return $ do
    ptr <- open "test"
    let getty = prg $ fget ptr
        setty = fput ptr

    while (return $ litExp True)
          (do v <- getty
              setty v
          )

    close ptr

type Prg a = Program (CMD Expr) (Expr a)

compile :: (Typeable a, Typeable b)
          =>    (Sig a -> Sig b)
          -> IO (Prg a -> Prg b)
compile f = SC.compile (S.unSig . f . S.Sig)

-}
