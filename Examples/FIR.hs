module FIR where

import Prelude hiding (break)

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
-- * Misc
--------------------------------------------------------------------------------

for :: Expr Int -> Expr Int -> (Expr Int -> Program (CMD Expr) ()) -> Program (CMD Expr) ()
for lo hi body = do
    ir <- newRef lo
    while
        (do i <- unsafeGetRef ir; return (leq i hi))
        (do i <- unsafeGetRef ir
            a <- body i
            setRef ir (i+1)
            return a
        )
  -- unsafeGetRef is fine because writing to the reference is the last thing that happens in each
  -- iteration

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
  B.cgen $ mkFunction "main" f

main :: IO ()
main = do
    code <- test
    writeFile "fir.c" $ show code

runFIR :: IO ()
runFIR = testF >>= runProgram

testF :: IO (Program (CMD Expr) ())
testF = do
  prg <- SC.compile (iir [1.1, 1.2, 1.3] [2.1, 2.2, 2.3])
  return $ do
    inp  <- open "input"
    outp <- open "output"
    let (Stream init) = prg $ do
          i     <- fget inp
          isEOF <- feof inp
          iff isEOF break (return ())
            -- Apparently EOF can only be detected after one has tried to read past the end
          return i
    let setty = fput outp
    getty <- init

    while (return $ litExp True)
          (do v <- getty
              setty v)

    close inp
    close outp
