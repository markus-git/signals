{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ConstraintKinds #-}

module Filters where

import Language.VHDL
import Language.Embedded.VHDL
import qualified Language.Embedded.VHDL.Expression as E

import Signal hiding (E)
import Signal.Compiler.Interface

import Control.Monad.Operational.Compositional
import Data.Bits
import Data.Typeable
import Data.Word
import System.IO

import Prelude hiding (repeat, map, zipWith, div)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | The kinds of commands we can use
type CMD exp = SequentialCMD exp :+: ConcurrentCMD exp :+: HeaderCMD exp

-- | Short-hand for the expression type we will use
type E = Expr

-- | Short-hand for signals over our commands
type S = Sig (CMD E)

-- | ...
type Sr a = Str (CMD E) a

-- | ...
instance Compile Expr
  where
    literal = litE

--------------------------------------------------------------------------------
-- ** Some simple signal functions

type Ok a = (Rep a, Typeable a)

repeat :: Ok a => E a -> S a
repeat = lift0

map :: Ok a => (E a -> E a) -> S a -> S a
map f = lift1 f

zipWith :: Ok a => (E a -> E a -> E a) -> S a -> S a -> S a
zipWith f = lift2 f

--------------------------------------------------------------------------------
-- ** Some simple circuits

high, low :: E Bool
high = litE True
low  = litE False

inv :: S Bool -> S Bool
inv = lift1 E.not

and2 :: S Bool -> S Bool -> S Bool
and2 = zipWith E.and

-- | Recursiv signal example
toggle :: S Bool -> S Bool
toggle s = let out = high `delay` (inv out) in and2 s out

-- | Mux. example
multiplexer :: S Word8 -> S Word8
multiplexer sig = mux2 toggle (sig, zeroes)
  where
    zeroes = repeat 0
    toggle = repeat high

--------------------------------------------------------------------------------
-- ** Some filters

divs :: S Word8 -> S Word8 -> S Word8
divs = zipWith div

sums :: [S Word8] -> S Word8
sums = P.foldr1 (+)

muls :: [E Word8] -> [S Word8] -> [S Word8]
muls es = P.zipWith (*) (P.map repeat es)

delays :: [E Word8] -> S Word8 -> [S Word8]
delays es s = scanl (flip delay) s es

-- | Finite impulse response filter
fir :: [E Word8] -> S Word8 -> S Word8
fir as = sums . muls as . delays ds
  where
    ds = P.replicate (P.length as) 0

-- | Infinite impulse response filter
iir :: [E Word8] -> [E Word8] -> S Word8 -> S Word8
iir (a:as) bs s = o
  where
    u = fir bs s
    l = fir as $ delay 0 o
    o = (1 `divs` repeat a) * (u - l)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

type P = Program (CMD E)

compSF :: Ok a => (S a -> S a) -> E a -> IO (P ())
compSF f a =
  do prog <- compiler f
     return . void . run . prog . Stream . return . return $ a

--------------------------------------------------------------------------------

testRec :: IO ()
testRec = compSF toggle high >>= putStrLn . compile

testMux :: IO ()
testMux = compSF multiplexer 0 >>= putStrLn . compile

testFIR :: IO ()
testFIR = compSF (fir [1,2]) 0 >>= putStrLn . compile

testIIR :: IO ()
testIIR = compSF (iir [1,2] [3,4]) 0 >>= putStrLn . compile

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------
{-
write :: IO ()
write =
  do prog <- compSF (fir [1,2])
     writeFile "generated.vhdl" (compile prog)
-}
--------------------------------------------------------------------------------
