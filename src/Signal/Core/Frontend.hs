{-# LANGUAGE ConstraintKinds #-}

module Signal.Core.Frontend where

import Signal.Core (Signal(..), Symbol(..), Literal)
import Signal.Core.Witness
import qualified Signal.Core as S

import Control.Monad.Identity (Identity)

import Data.Bits
import Data.Typeable

import Prelude hiding (repeat, map, zipWith)

--------------------------------------------------------------------------------
-- * Signal front-end.
--------------------------------------------------------------------------------

-- | Wrapper for signals to hide 'Identity'.
newtype Sig exp pred a = Sig { runSig :: Signal exp pred (Identity a) }

-- | Constant value.
repeat :: pred a => exp a -> Sig exp pred a
repeat = Sig . S.repeat

-- | Delay a signal by one value.
delay :: (Literal exp pred, pred a) => a -> Sig exp pred a -> Sig exp pred a
delay e = Sig . S.delay e . runSig

-- | Apply a function element-wise to a signal.
map :: (pred a, pred b)
  => (exp a -> exp b)
  -> Sig exp pred a
  -> Sig exp pred b
map f = Sig . S.map f . runSig

-- | Join two signals.
zipWith :: (pred a, pred b, pred c)
  => (exp a -> exp b -> exp c)
  -> Sig exp pred a
  -> Sig exp pred b
  -> Sig exp pred c
zipWith f a b = Sig $ S.map (uncurry f) $ S.zip (runSig a) (runSig b)

--------------------------------------------------------------------------------

instance Eq (Sig exp pred a)
  where
    Sig (Signal (Symbol s1)) == Sig (Signal (Symbol s2)) = s1 == s2

instance (Bounded (exp a), pred a) => Bounded (Sig exp pred a)
  where
    minBound = repeat minBound
    maxBound = repeat maxBound

instance (Ord (exp a), pred a) => Ord (Sig exp pred a)
  where
    compare = error "compare is not suppored"
    max     = zipWith max
    min     = zipWith min

instance (Bits (exp a), pred a) => Bits (Sig exp pred a)
  where
    (.&.)        = zipWith (.&.)
    (.|.)        = zipWith (.|.)
    xor          = zipWith xor
    complement   = map complement 
    shift  s n   = map (flip shift n) s
    rotate s n   = map (flip rotate n) s
    bit          = repeat . bit
    testBit      = error "testBit is not supported"
    bitSize      = error "bitSize is not supported"
    bitSizeMaybe = error "bitSizeMaybe is not supported"
    isSigned     = error "isSigned is not supported"
    popCount     = error "popCound is not supported"

instance (Num (exp a), pred a) => Num (Sig exp pred a)
  where
    (+)         = zipWith (+)
    (-)         = zipWith (-)
    (*)         = zipWith (*)
    negate      = map negate
    abs         = map abs
    signum      = map signum
    fromInteger = repeat . fromInteger

instance (Fractional (exp a), pred a) => Fractional (Sig exp pred a)
  where
    (/)          = zipWith (/)
    recip        = map recip
    fromRational = repeat . fromRational

instance (Integral (exp a), pred a) => Integral (Sig exp pred a)
  where
    quot      = zipWith quot
    rem       = zipWith rem
    quotRem a b =
      let signal = S.map (uncurry quotRem) $ S.zip (runSig a) (runSig b)
          left   = Sig $ S.fst signal
          right  = Sig $ S.snd signal
      in (left, right)
    divMod  a b =
      let signal = S.map (uncurry divMod) $ S.zip (runSig a) (runSig b)
          left   = Sig $ S.fst signal
          right  = Sig $ S.snd signal
      in (left, right)
    toInteger = error "toIntegral not supported"

instance Enum (Sig exp pred a)
  where
    toEnum   = error "toEnum not supported"
    fromEnum = error "fromEnum not supported"

instance (Num (exp a), Ord (exp a), pred a) => Real (Sig exp pred a)
  where
    toRational = error "toRational not supported"

--------------------------------------------------------------------------------
