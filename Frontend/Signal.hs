{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ConstraintKinds     #-}

module Frontend.Signal where

import Expr (Expr, todo)

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
data Leaf a deriving Typeable

-- |
data Signal a
  where
    Const :: Typeable a
            => Stream a
            -> Signal (Leaf a)

    Lift  :: (Typeable a, Typeable b)
            => (Stream a        -> Stream b)
            -> (Signal (Leaf a) -> Signal (Leaf b))

    Map   :: (Typeable a, Typeable b)
            => (Struct a -> Struct b)
            -> (Signal a -> Signal b)

    Zip   :: (Typeable a, Typeable b) => Signal a      -> Signal b -> Signal (a, b)
    Fst   :: Typeable a               => Signal (a, b) -> Signal a
    Snd   :: Typeable b               => Signal (a, b) -> Signal b

    Delay :: a -> Signal (Leaf a) -> Signal (Leaf a)

    SVar  :: Typeable a => Dynamic -> Signal a
  deriving Typeable

-- | ...
newtype Sig a = Sig {unSig :: Signal (Leaf a)}

--------------------------------------------------------------------------------
-- ** "Smart" constructors

constS :: Typeable a => Stream a -> Sig a
constS  = Sig . Const

liftS  :: (Typeable a, Typeable b) => (Stream a -> Stream b) -> Sig a -> Sig b
liftS f = Sig . Lift f . unSig

mapS   :: (Typeable a, Typeable b) => (Struct a -> Struct b) -> Signal a -> Signal b
mapS    = Map

zipS   :: (Typeable a, Typeable b) => Signal a -> Signal b -> Signal (a, b)
zipS    = Zip

fstS   :: Typeable a => Signal (a, b) -> Signal a
fstS    = Fst

sndS   :: Typeable b => Signal (a, b) -> Signal b
sndS    = Snd

--------------------------------------------------------------------------------
-- ** User Interface

repeat :: (Typeable1 exp, Typeable a) => exp a -> Sig (exp a)
repeat = constS . S.repeat

map :: (Typeable1 exp, Typeable a, Typeable b)
    => (exp a -> exp b)
    -> Sig (exp a)
    -> Sig (exp b)
map f = liftS (S.map f)

zipWith :: (Typeable1 exp, Typeable c, Typeable b, Typeable a)
        => (exp a -> exp b -> exp c)
        -> Sig (exp a)
        -> Sig (exp b)
        -> Sig (exp c)
zipWith f = curry $ lift $ uncurry f

--------------------------------------------------------------------------------
-- ** Instances

instance ( Show a
         , Typeable a
         , Typeable1 expr
         , Num (expr a))
    => Num (Sig (expr a))
  where
    fromInteger = repeat . fromInteger
    (+)         = zipWith (+)
    (*)         = zipWith (*)
    (-)         = zipWith (-)

    abs = todo; signum = todo;

instance ( Show a
         , Typeable a
         , Typeable1 expr
         , Fractional (expr a))
    => Fractional (Sig (expr a))
  where
    fromRational = repeat . fromRational
    (/)          = zipWith (/)

    recip = todo;

instance ( Show a
         , Typeable a
         , Typeable1 expr
         , Floating (expr a))
    => Floating (Sig (expr a))
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
-- *
--------------------------------------------------------------------------------
-- {- All this to convert between tuples and zip pairs -}

class StructS a
  where
    type Internal a

    fromS :: a -> Signal (Internal a)
    toS   :: Signal (Internal a) -> a

instance StructS (Signal (Leaf a))
  where
    type Internal (Signal (Leaf a)) = Leaf a

    fromS = id
    toS   = id

instance StructS (Sig a)
  where
    type Internal (Sig a) = Leaf a

    fromS = unSig
    toS   = Sig

instance ( Typeable (Internal a)
         , Typeable (Internal b)
         , StructS a, StructS b)
    => StructS (a, b)
  where
    type Internal (a, b) = (Internal a, Internal b)

    fromS (a, b) = zipS (fromS a) (fromS b)
    toS p        = (toS (fstS p), toS (sndS p))

--------------------------------------------------------------------------------
-- ***

data Struct a
  where
    Leaf :: a                    -> Struct (Leaf a)
    Pair :: Struct a -> Struct b -> Struct (a, b)
  deriving Typeable

class StructE a
  where
    type Normal a

    fromE :: Struct a -> Normal a
    toE   :: Normal a -> Struct a

instance StructE (Leaf a)
  where
    type Normal (Leaf a) = a
    fromE (Leaf a) = a
    toE a          = Leaf a

instance (StructE a, StructE b)
    => StructE (a, b)
  where
    type Normal (a, b) = (Normal a, Normal b)

    fromE (Pair a b) = (fromE a, fromE b)
    toE (a, b)       = Pair (toE a) (toE b)

--------------------------------------------------------------------------------
-- ***

lift
  :: ( StructS s1
     , StructS s2
     , StructE (Internal s1)
     , StructE (Internal s2)
     , Typeable (Internal s1)
     , Typeable (Internal s2)
     )
  => (Normal (Internal s1) -> Normal (Internal s2)) -> s1 -> s2
lift f = toS . mapS (toE . f . fromE) . fromS
