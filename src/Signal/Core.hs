{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Signal.Core
  ( Sig   (..)
  , Signal(..)
  , Symbol(..)
  , S     (..)
  , E    
  , variable
  , delay
  , lift
  , lift0
  , lift1
  , lift2
  ) where

import Signal.Core.Witness

import Control.Monad.Operational.Compositional hiding (join)
import Control.Monad.Identity                  hiding (join)
import Data.Bits
import Data.Typeable          (Typeable)
import Data.Dynamic           (Dynamic)
import Data.Ref
import Data.Unique
import Language.Embedded.VHDL.Interface

import Prelude hiding (Left, Right, map, repeat)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Signals
--------------------------------------------------------------------------------

-- | ...
newtype Sig    i a = Sig { runSig :: Signal i (Identity a) }

-- | ...
newtype Signal i a = Signal { runSignal :: Symbol i a }

-- | ...
newtype Symbol i a = Symbol (Ref (S Symbol i a))

-- | ...
data S sig i a
  where
    -- ^ Constant signals
    Repeat :: (Typeable a, PredicateExp (IExp i) a) => IExp i a -> S sig i (Identity a)

    -- ^ Signal transformers
    Map    :: (Witness i a, Witness i b) => (E i a -> E i b) -> sig i a -> S sig i b

    -- ^ Wiring operators
    Join   :: (Witness i a, Witness i b) => sig i a -> sig i b -> S sig i (a, b)
    Left   :: (Witness i a, Witness i b) => sig i (a, b) -> S sig i a
    Right  :: (Witness i a, Witness i b) => sig i (a, b) -> S sig i b

    -- ^ Registers
    Delay  :: (Typeable a, PredicateExp (IExp i) a)
           => IExp i a
           -> sig i (Identity a)
           -> S sig i (Identity a)

    -- ^ Multiplexers
    Mux    :: (Typeable a, PredicateExp (IExp i) a, Witness i b)
           => sig i (Identity a)
           -> [(a, sig i b)]
           -> S sig i b

    -- ^ Variable trick
    Var    :: Witness i a => Dynamic -> S sig i a

-- | Type for nested tuples, stripping away all Identity
type family E i a
  where
    E i (Identity a) = IExp i a
    E i (a, b)       = (E i a, E i b)

--------------------------------------------------------------------------------
-- ** Some `smart` constructions

signal :: S Symbol i a -> Signal i a
signal = Signal . symbol

symbol :: S Symbol i a -> Symbol i a
symbol = Symbol . ref

unsymbol :: Symbol i a -> S Symbol i a
unsymbol (Symbol s) = deref s

--------------------------------------------------------------------------------
-- internal

repeat :: (Typeable a, PredicateExp (IExp i) a) => IExp i a -> Signal i (Identity a)
repeat s = signal $ Repeat s

map :: (Witness i a, Witness i b) => (E i a -> E i b) -> Signal i a -> Signal i b
map f (Signal s) = signal $ Map f s

join :: (Witness i a, Witness i b) => Signal i a -> Signal i b -> Signal i (a, b)
join (Signal a) (Signal b) = signal $ Join a b

left :: (Witness i a, Witness i b) => Signal i (a, b) -> Signal i a
left (Signal s) = signal $ Left s

right :: (Witness i a, Witness i b) => Signal i (a, b) -> Signal i b
right (Signal s) = signal $ Right s

variable :: Witness i a => Dynamic -> Signal i a
variable = signal . Var 

--------------------------------------------------------------------------------
-- user

-- | Delay a signal by one instant, returning the given value in the first instant
delay :: (Typeable a, PredicateExp (IExp i) a) => IExp i a -> Sig i a -> Sig i a
delay e (Sig (Signal s)) = Sig . signal $ Delay e s

-- | Choose output signal according to a control signal
--
-- ^ List must be total, covering all cases
-- ^ ...
mux :: ( Typeable a, PredicateExp (IExp i) a
       , Typeable b, PredicateExp (IExp i) b)
    => Sig i a
    -> [(a, Sig i b)]
    -> Sig i b
mux (Sig (Signal s)) = Sig . signal . Mux s . fmap (fmap (runSignal . runSig))

--------------------------------------------------------------------------------
-- ** Properties of signals

instance Eq (Sig i a)
  where
    Sig (Signal (Symbol s1)) == Sig (Signal (Symbol s2)) = s1 == s2

instance (Bounded (IExp i a), Typeable a, PredicateExp (IExp i) a) => Bounded (Sig i a)
  where
    minBound = lift0 minBound
    maxBound = lift0 maxBound

instance (Ord (IExp i a), Typeable a, PredicateExp (IExp i) a) => Ord (Sig i a)
  where
    compare = error "compare is not suppored"
    max     = lift2 max
    min     = lift2 min

instance (Enum (IExp i a), Typeable a, PredicateExp (IExp i) a) => Enum (Sig i a) -- needed for integral
  where
    toEnum   = error "toEnum not supported"
    fromEnum = error "fromEnum not supported"

instance (Bits (IExp i a), Typeable a, PredicateExp (IExp i) a) => Bits (Sig i a)
  where
    (.&.)        = lift2 (.&.)
    (.|.)        = lift2 (.|.)
    xor          = lift2 xor
    complement   = lift1 complement 
    shift  s n   = lift1 (flip shift n) s
    rotate s n   = lift1 (flip rotate n) s
    bit          = lift0 . bit
    testBit      = error "testBit is not supported.. yet"
    bitSize      = error "bitSize is not supported.. yet"
    bitSizeMaybe = error "bitSizeMaybe is not supported.. yet"
    isSigned     = error "isSigned is not supported.. yet"
    popCount     = error "popCound is not supported.. yet"
    
instance (Num (IExp i a), Typeable a, PredicateExp (IExp i) a) => Num (Sig i a)
  where
    (+)         = lift2 (+)
    (-)         = lift2 (-)
    (*)         = lift2 (*)
    negate      = lift1 negate
    abs         = lift1 abs
    signum      = lift1 signum
    fromInteger = lift0 . fromInteger

instance (Fractional (IExp i a), Typeable a, PredicateExp (IExp i) a) => Fractional (Sig i a)
  where
    (/)          = lift2 (/)
    recip        = lift1 recip
    fromRational = lift0 . fromRational

instance (Real (IExp i a), Typeable a, PredicateExp (IExp i) a) => Real (Sig i a)
  where
    toRational = error "toRational not supported"

instance (Integral (IExp i a), Typeable a, PredicateExp (IExp i) a) => Integral (Sig i a)
  where
    quot      = lift2 quot
    rem       = lift2 rem
    quotRem   = curry $ lift p $ uncurry quotRem
      where p = undefined :: proxy i (Identity a, Identity a) (Identity a, Identity a)
    toInteger = error "toIntegral not supported"

--------------------------------------------------------------------------------
-- * Nested Signals
--------------------------------------------------------------------------------

-- | Type for nested tuples of signals
type family Packed (i :: (* -> *) -> * -> *) a :: *
type instance Packed i (Identity a) = Sig i a
type instance Packed i (a, b)       = (Packed i a, Packed i b)

pack :: forall i a. Witness i a => Signal i a -> Packed i a
pack s = go (witness :: Wit i a) s
  where
    go :: Wit i x -> Signal i x -> Packed i x
    go (WE)     s = Sig s
    go (WP u v) s = (,) (go u $ left s) (go v $ right s)

unpack :: forall i a. Witness i a => Packed i a -> Signal i a
unpack s = go (witness :: Wit i a) s
  where
    go :: Wit i x -> Packed i x -> Signal i x
    go (WE)     (Sig s) = s
    go (WP u v) (l, r)  = join (go u l) (go v r)

--------------------------------------------------------------------------------
-- ** General lifting operator

-- todo: I don't like the proxy, or the type family. Possible to find E^(-1)?
--     : can use injective type functions to get rid of proxy.
lift 
  :: forall proxy i a b. (Witness i a, Witness i b)
  => proxy i a b
  -> (E i a      -> E i b)
  -> (Packed i a -> Packed i b)
lift _ f = pack . (map f :: Signal i a -> Signal i b) . unpack

--------------------------------------------------------------------------------
-- * Some common signal operations
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Multiplexing

mux2
  :: ( Typeable a
     , PredicateExp (IExp i) a
     , PredicateExp (IExp i) Bool)
  => Sig i Bool
  -> (Sig i a, Sig i a)
  -> Sig i a
mux2 b (t, f) = mux b [(True, t), (False, f)]

--------------------------------------------------------------------------------
-- ** Lifting

lift0 :: (Typeable a, PredicateExp e a, e ~ IExp i) => e a -> Sig i a
lift0 = Sig . repeat

lift1
  :: forall i e a b.
     ( Typeable a, PredicateExp e a
     , Typeable b, PredicateExp e b
     , e ~ IExp i)
  => (e a -> e b)
  -> Sig i a
  -> Sig i b
lift1 f = lift p f
  where
    p = undefined :: proxy i (Identity a) (Identity b)

lift2
  :: forall i e a b c.
     ( Typeable a, PredicateExp e a
     , Typeable b, PredicateExp e b
     , Typeable c, PredicateExp e c
     , e ~ IExp i)
  => (e a -> e b -> e c)
  -> Sig i a
  -> Sig i b
  -> Sig i c
lift2 f = curry $ lift p $ uncurry f
  where
    p = undefined :: proxy i (Identity a, Identity b) (Identity c)

--------------------------------------------------------------------------------
-- the end.
