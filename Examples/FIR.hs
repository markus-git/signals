module FIR where

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
-- * FIR Filter Example
--------------------------------------------------------------------------------

fir :: [Expr Float] -> Sig Float -> Sig Float
fir as = sums . muls as . delays ds
  where ds = replicate (length as) 0

sums :: [Sig Float] -> Sig Float
sums = foldr1 (+)

muls :: [Expr Float] -> [Sig Float] -> [Sig Float]
muls as = zipWith (*) (map S.repeat as)

delays :: [Expr Float] -> Sig Float -> [Sig Float]
delays as s = tail $ scanl (flip S.delay) s as

--------------------------------------------------------------------------------
-- * IIR Filter Examples
--------------------------------------------------------------------------------

iir :: [Expr Float] -> [Expr Float] -> Sig Float -> Sig Float
iir as bs s = o
  where
    u = fir bs  s
    l = fir as' o  where as' = map negate as
    o = u + l

--------------------------------------------------------------------------------

test :: IO Doc
test = do
  f <- testF
  B.cgen $ mkFunction "misc" f

testF :: IO (Program (CMD Expr) ())
testF = do
  prg <- compile (fir [1.1, 1.2, 1.3])
  return $ do
    ptr <- open "test"
    let getty = prg $ fget ptr
        setty = fput ptr

    v <- getty
    setty v

    close ptr

testFF :: IO (Program (CMD Expr) ())
testFF = do
  prg <- compile (iir [2.1, 2.2] [1.1, 1.2, 1.3])
  return $ do
    ptr <- open "test"
    let getty = prg $ fget ptr
        setty = fput ptr

    v <- getty
    setty v

    close ptr

--------

type Prg a = Program (CMD Expr) (Expr a)

compile :: (Typeable a, Typeable b)
          =>    (Sig a -> Sig b)
          -> IO (Prg a -> Prg b)
compile f = SC.compile (S.unSig . f . S.Sig)
