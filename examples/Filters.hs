{-# LANGUAGE TypeOperators #-}

module Filters where

import Signal
import Language.VHDL
import Language.Embedded.VHDL
import Control.Monad.Operational.Compositional

import Prelude hiding (repeat, map, zipWith, div)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | The kinds of commands we can use
type CMD exp = ConcurrentCMD exp :+: HeaderCMD exp

-- | Short-hand for the expression type we will use
type E = Expr

-- | Short-hand for signals over our commands
type S = Sig (CMD E)

--------------------------------------------------------------------------------

repeat :: E Float -> S Float
repeat = lift0

map :: (E Float -> E Float) -> S Float -> S Float
map f = lift1 f

zipWith :: (E Float -> E Float -> E Float) -> S Float -> S Float -> S Float
zipWith f = lift2 f

--------------------------------------------------------------------------------

divs :: S Float -> S Float -> S Float
divs = zipWith div

sums :: [S Float] -> S Float
sums = P.foldr1 (+)

muls :: [E Float] -> [S Float] -> [S Float]
muls es = P.zipWith (*) (P.map repeat es)

delays :: [E Float] -> S Float -> [S Float]
delays es s = scanl (flip delay) s es

--------------------------------------------------------------------------------
-- ** Some filters

fir :: [E Float] -> S Float -> S Float
fir as = sums . muls as . delays ds
  where
    ds = P.replicate (P.length as) 0

iir :: [E Float] -> [E Float] -> S Float -> S Float
iir (a:as) bs s = o
  where
    u = fir bs s
    l = fir as $ delay 0 o
    o = (1 `divs` repeat a) * (u - l)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

type P = Program (CMD E)

compSF :: (S Float -> S Float) -> IO (P ())
compSF f =
  do prog <- compiler $ f $ repeat 1 -- temp
     return $ void $ run prog

compFIR :: IO ()
compFIR =
  do prog <- compSF (fir [1,2])
     putStrLn $ compile prog

compIIR :: IO ()
compIIR =
  do prog <- compSF (iir [1,2] [2,1])
     putStrLn $ compile prog

--------------------------------------------------------------------------------
