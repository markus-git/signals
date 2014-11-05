{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs         #-}

module Frontend.Signal where

import Expr
import Interpretation

import           Frontend.Stream (Stream)
import qualified Frontend.Stream as S

import           Prelude hiding (fst, snd, zip, zipWith, map, repeat)
import qualified Prelude as P

import Data.Dynamic
import Data.Typeable

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- |
data Signal a
  where
    Const :: Typeable a
            => Stream (Expr a)
            -> Signal (Expr a)

    Lift  :: (Typeable a, Typeable b)
            => (Stream (Expr a) -> Stream (Expr b))
            -> (Signal (Expr a) -> Signal (Expr b))

    Map   :: (Typeable a, Typeable b)
            => (Struct a -> Struct b)
            -> (Signal a -> Signal b)

    Zip   :: (Typeable a, Typeable b) => Signal a      -> Signal b -> Signal (a, b)
    Fst   :: (Typeable a)             => Signal (a, b)             -> Signal a
    Snd   :: (Typeable b)             => Signal (a, b)             -> Signal b

    Delay :: Expr a -> Signal (Expr a) -> Signal (Expr a)

    SVar  :: (Typeable a) => Dynamic -> Signal a
  deriving Typeable

-- | ...
--
-- The use of Expr here is only temporary (due to a Typeable thingy),
-- with GHC >7.8.* exp can be in the type instead
newtype Sig a = Sig {unSig :: Signal (Expr a)}

--------------------------------------------------------------------------------
-- ** "Smart" constructors

constS :: (Typeable a)
         => Stream (Expr a)
         -> Sig a
constS  = Sig . Const

liftS  :: (Typeable a, Typeable b)
         => (Stream (Expr a) -> Stream (Expr b))
         -> (Sig a -> Sig b)
liftS f = Sig . Lift f . unSig

mapS   :: (Typeable a, Typeable b)
         => (Struct a -> Struct b)
         -> (Signal a -> Signal b)
mapS    = Map

zipS   :: (Typeable a, Typeable b) => Signal a -> Signal b -> Signal (a, b)
zipS    = Zip

fstS   :: (Typeable a) => Signal (a, b) -> Signal a
fstS    = Fst

sndS   :: (Typeable b) => Signal (a, b) -> Signal b
sndS    = Snd

--------------------------------------------------------------------------------
-- ** User Interface

repeat :: (Typeable a) => Expr a -> Sig a
repeat = constS . S.repeat

map :: (Typeable a, Typeable b)
      => (Expr a -> Expr b)
      -> Sig a
      -> Sig b
map f = liftS (S.map f)

zipWith :: (Typeable a, Typeable b, Typeable c)
          => (Expr a -> Expr b -> Expr c)
          -> Sig a
          -> Sig b
          -> Sig c
zipWith f = curry $ lift $ uncurry f

--------------------------------------------------------------------------------
-- ** Instances

instance (Show a, Typeable a, Num a) => Num (Sig a)
  where
    fromInteger = repeat . fromInteger
    (+)         = zipWith (+)
    (*)         = zipWith (*)
    (-)         = zipWith (-)

    abs = todo; signum = todo;

instance (Show a, Typeable a, Fractional a) => Fractional (Sig a)
  where
    fromRational = repeat . fromRational
    (/)          = zipWith (/)

    recip = todo;

instance (Show a, Typeable a, Floating a) => Floating (Sig a)
  where
    pi   = repeat pi
    sin  = map sin
    (**) = zipWith (**)

    exp   = todo; sqrt  = todo; log     = todo;
    tan   = todo; cos   = todo; asin    = todo;
    atan  = todo; acos  = todo; sinh    = todo;
    tanh  = todo; cosh  = todo; asinh   = todo;
    atanh = todo; acosh = todo; logBase = todo;

--------------------------------------------------------------------------------
-- * Generalised lifting of Signals
--------------------------------------------------------------------------------

-- {- All this to convert between tuples and zip pairs -}

--------------------------------------------------------------------------------
-- *** Conversion between signals and tuples

class StructS a
  where
    type Internal a

    fromS :: a -> Signal (Internal a)
    toS   :: Signal (Internal a) -> a

instance StructS (Signal (Expr a))
  where
    type Internal (Signal (Expr a)) = Expr a

    fromS = id
    toS   = id

instance StructS (Sig a)
  where
    type Internal (Sig a) = Expr a

    fromS = unSig
    toS   = Sig

instance ( Typeable (Internal a)
         , Typeable (Internal b)
         , StructS a
         , StructS b
         )
    => StructS (a, b)
  where
    type Internal (a, b) = (Internal a, Internal b)

    fromS (a, b) = zipS (fromS a) (fromS b)
    toS p        = (toS (fstS p), toS (sndS p))

--------------------------------------------------------------------------------
-- *** Conversion between struct's and tuples

class StructE a
  where
    type Normal a

    fromE :: Struct a -> Normal a
    toE   :: Normal a -> Struct a

instance Typeable a => StructE (Expr a)
  where
    type Normal (Expr a) = Expr a

    fromE (Leaf a) = a
    toE a          = Leaf a

instance (StructE a, StructE b) => StructE (a, b)
  where
    type Normal (a, b) = (Normal a, Normal b)

    fromE (Pair a b) = (fromE a, fromE b)
    toE (a, b)       = Pair (toE a) (toE b)

--------------------------------------------------------------------------------
-- *** The lifting operator

lift
  :: ( StructS            s1,  StructS            s2
     , StructE  (Internal s1), StructE  (Internal s2)
     , Typeable (Internal s1), Typeable (Internal s2)
     )
  => (Normal (Internal s1) -> Normal (Internal s2)) -> s1 -> s2
lift f = toS . mapS (toE . f . fromE) . fromS
