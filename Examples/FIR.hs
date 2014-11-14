module FIR where

import Core
import Expr
import Interpretation

import           Frontend.Signal     (Sig)
import qualified Frontend.Signal     as S

import           Frontend.Stream     (Stream(..))
import qualified Frontend.Stream     as Str

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
  prg <- SC.compile (fir [1.1, 1.2, 1.3])
  return $ do
    ptr   <- open "test"
    let (Stream init) = prg $ fget ptr
    let setty = fput ptr
    getty <- init

    while (return $ litExp True)
          (do v <- getty
              setty v)

    close ptr
