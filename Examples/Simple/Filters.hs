module Filters where

import Prelude hiding (break)

import Core
import Interpretation

import Examples.Simple.Expr
import Frontend.Signal (Sig)
import Frontend.Stream (Str, Stream(..))
import Backend.Compiler.Compiler
import qualified Frontend.Signal as S
import qualified Frontend.Stream as Str
import qualified Backend.C       as B

import Control.Monad
import Control.Monad.Operational (Program)
import Text.PrettyPrint.Mainland
import Data.IORef
import Data.Array.IO.Safe
import qualified System.IO as IO
import qualified Text.Printf as Printf

--------------------------------------------------------------------------------
-- * Misc Types
--------------------------------------------------------------------------------

type E = Expr

type S = Sig E

type P = Program (CMD E)

--------------------------------------------------------------------------------

-- | classical for loop
for :: E Int -> E Int -> (E Int -> P ()) -> P ()
for lo hi body = do
    ir <- newRef lo
    while
        (do i <- unsafeGetRef ir; return (leq i hi))
        (do i <- unsafeGetRef ir
            a <- body i
            setRef ir (i+1)
            return a
        )
  -- unsafeGetRef is fine because writing to the reference is the last thing
  -- that happens in each iteration

--------------------------------------------------------------------------------
-- * FIR Filter Example
--------------------------------------------------------------------------------

fir :: [E Float] -> S Float -> S Float
fir as = sums . muls as . delays ds
  where ds = replicate (length as) 0

sums :: [S Float] -> S Float
sums = foldr1 (+)

muls :: [E Float] -> [S Float] -> [S Float]
muls as = zipWith (*) (map S.repeat as)

delays :: [E Float] -> S Float -> [S Float]
delays as s = scanl (flip S.delay) s as

--------------------------------------------------------------------------------
-- * IIR Filter Examples
--------------------------------------------------------------------------------

iir :: [E Float] -> [E Float] -> S Float -> S Float
iir (a:as) bs s = o
  where
    u = fir bs s
    l = fir as $ S.delay 0 o
    o = (1 / S.repeat a) * (u - l)

--------------------------------------------------------------------------------
-- * FFT Filter Examples
--------------------------------------------------------------------------------

-- todo

--------------------------------------------------------------------------------
-- * Testing of filters
--------------------------------------------------------------------------------

-- for eval you will need to make sure there is an input file, called "input",
-- to read from. Its a standard file of numbers seperated by a space.

test_fir = comp (fir [1,2])
eval_fir = eval (fir [1,2])

test_iir = comp (iir [1,2] [3,4]) -- crashes! why?!..
eval_iir = eval (iir [1,2] [3,4])

--------------------------------------------------------------------------------

-- |
eval :: (S Float -> S Float) -> IO ()
eval = connect_io >=> B.runProgram

-- | ...
comp :: (S Float -> S Float) -> IO Doc
comp = connect_io >=> B.cgen . mkFunction "main"

--------------------------------------------------------------------------------

connect_io :: (S Float -> S Float) -> IO (P ())
connect_io s = do
  prg <- compiler s
  return $ do
    inp  <- open "input"
    outp <- open "output"

    let (Stream init) = prg $ Str.stream $ return $ do
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
{-
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
-}
