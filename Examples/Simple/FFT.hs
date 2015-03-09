module Examples.Simple.FFT where

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


import Data.Complex

--------------------------------------------------------------------------------
-- * Misc Types
--------------------------------------------------------------------------------

type E = Expr

type S = Sig E

type P = Program (CMD E)


--------------------------------------------------------------------------------
-- * Playing with signals
--------------------------------------------------------------------------------

oneComplex :: Expr (Complex Double)
oneComplex = 1.0

onesSig :: S (Complex Double)
onesSig = S.repeat oneComplex

--------------------------------------------------------------------------------
-- * FFT
--------------------------------------------------------------------------------

-- Construct function operating on 2n-lists from function operating on n-list

-- Applies a function to first part of splitted list, leaving second unchanged
one :: ([a] -> [a]) -> ([a] -> [a])
one f l = (f l) ++ (l)

-- Applies a function to both parts of a splitted list
two :: ([a] -> [b]) -> ([a] -> [b])
two f l = (f l) ++ (f l)

-- Applies the function f n times
raised :: Int -> (a -> a) -> (a -> a)
raised n f = (!! n) . iterate f

{-
(>->) :: Circuit m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- I don't think we need this - we can use function composition instead
-}


bfly :: [S (Complex Double)] -> [S (Complex Double)]
bfly [i1, i2] = [i1 + i2, i1 - i2]

bflys :: Int -> [S (Complex Double)] -> [S (Complex Double)]
bflys n = unriffle . raised n two bfly . riffle

w :: Int -> Int -> Int 
w n 0 = 1
w n k
   | k == n    = 1
   | otherwise = w (2 * n) (2 * k)
 
 
cplus :: S (Complex Double) -> S (Complex Double) -> S (Complex Double)
cplus x y = x + y

ctimes :: Int -> S (Complex Double) -> S (Complex Double)
ctimes a x = aSig * x
  where aCplx = 1.0 :: Expr (Complex Double)
        aSig  = (S.repeat aCplx)
{-
ctimes a x = ctimes' a x x
ctimes' a x orig
  | a == 0 = error
  | a == 1 = x
  | otherwise = ctimes' (a - 1) (cplus x orig) orig 
-}

wMult :: Int -> Int -> S (Complex Double) -> S (Complex Double)
wMult n k a = ctimes twiddleFactor a
  where twiddleFactor = w n k

{-
minusJ :: [S (Complex Double)] -> [S (Complex Double)]
--minusJ = map (wMult 4 1)
minusJ = undefined
-}


-- TODO: split in ut in only two?
-- Split what in two?

splitTwo :: [a] -> ([a], [a])
splitTwo xs = ( take len xs, drop len xs )
    where len = length xs `div` 2

-- how do splitTwo and unriffle relate to each other?

riffle :: [a] -> [a]
riffle = riffle' . splitTwo

riffle' :: ([a], [a]) -> [a]
riffle' ([], [])         = []
riffle' ((x:xs), (y:ys)) = x : y : riffle' (xs, ys)

unriffle :: [a] -> [a]
unriffle xs = as ++ bs
  where (as,bs) = unriffle' xs ([], [])

unriffle' :: [a] -> ([a], [a]) -> ([a], [a])
unriffle' []        acc    = acc
unriffle' (x:y:xs)  (p, q) = unriffle' xs (p ++ [x], q ++ [y])      --- inefficient, but leave for now
unriffle' _          _     = error "illegal input"


-- TODO: other functions missing, see paper
