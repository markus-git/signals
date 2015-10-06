{-# LANGUAGE TypeOperators #-}

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

repeat :: (Rep a, Typeable a) => E a -> S a
repeat = lift0

map :: (E Word8 -> E Word8) -> S Word8 -> S Word8
map f = lift1 f

zipWith :: (E Word8 -> E Word8 -> E Word8) -> S Word8 -> S Word8 -> S Word8
zipWith f = lift2 f

--------------------------------------------------------------------------------
-- ** Some simple circuits

high, low :: E Bool
high = litE True
low  = litE False

inv :: S Bool -> S Bool
inv = lift1 E.not

multiplexer :: S Word8 -> S Word8
multiplexer sig = mux2 toggle (sig, zeroes)
  where
    zeroes = repeat 0
    toggle = repeat high --high `delay` (inv toggle)

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

compSF :: (S Word8 -> S Word8) -> IO (P ())
compSF f =
  do prog <- compiler f
     return $ void $ run $ prog $ (Stream . return $ return 0)

--------------------------------------------------------------------------------

testMux :: IO ()
testMux = compSF (multiplexer) >>= putStrLn . compile

testFIR :: IO ()
testFIR = compSF (fir [1,2]) >>= putStrLn . compile

testIIR :: IO ()
testIIR = compSF (iir [1,2] [3,4]) >>= putStrLn . compile

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

write :: IO ()
write =
  do prog <- compSF (fir [1,2])
     writeFile "generated.vhdl" (compile prog)

--------------------------------------------------------------------------------
