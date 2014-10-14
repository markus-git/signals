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
data Signal a
  where
    Const :: Typeable a =>  Stream a -> Signal (Leaf a)
    Lift  :: Typeable b
            => (Stream a        -> Stream b)
            -> (Signal (Leaf a) -> Signal (Leaf b))

    Map   :: Typeable b => (Struct a -> Struct b) -> Signal a -> Signal b

    Zip   ::               Signal a      -> Signal b -> Signal (a, b)
    Fst   :: Typeable a => Signal (a, b) -> Signal a
    Snd   :: Typeable b => Signal (a, b) -> Signal b

    Delay :: a -> Signal (Leaf a) -> Signal (Leaf a)

    Var   :: Typeable a => Dynamic -> Signal a
  deriving Typeable

-- |
data Leaf a deriving Typeable

--------------------------------------------------------------------------------
-- ** "Smart" constructors

-- | ...
newtype Sig a = Sig { unSig :: Signal (Leaf a) }

constSig :: Typeable a => Stream a -> Sig a
constSig  = Sig . Const

liftSig  :: Typeable b => (Stream a -> Stream b) -> Sig a -> Sig b
liftSig f = Sig . Lift f . unSig

mapSig   :: Typeable b => (Struct a -> Struct b) -> Signal a -> Signal b
mapSig    = Map

----------------------------------------

data WitType a
  where
    Wit :: Typeable a => WitType a

witTypeable :: Signal a -> WitType a
witTypeable (Const _)      = Wit
witTypeable (Lift _ _)     = Wit
witTypeable (Map _ a)
    | Wit <- witTypeable a = Wit
witTypeable (Zip a b)
    | Wit <- witTypeable a
    , Wit <- witTypeable b = Wit
witTypeable (Fst _)        = Wit
witTypeable (Snd _)        = Wit
witTypeable (Delay _ a)
    | Wit <- witTypeable a = Wit
witTypeable (Var _)        = Wit

--------------------------------------------------------------------------------
-- ** User Interface

repeat :: (Typeable1 exp, Typeable a) => exp a -> Sig (exp a)
repeat = constSig . S.repeat

map :: (Typeable1 exp, Typeable b) => (exp a -> exp b) -> Sig (exp a) -> Sig (exp b)
map f = liftSig (S.map f)

zipWith :: (Typeable1 exp, Typeable c, Typeable b, Typeable a)
        => (exp a -> exp b -> exp c)
        -> Sig (exp a)
        -> Sig (exp b)
        -> Sig (exp c)
zipWith f = curry $ lift $ uncurry f

zip :: Signal a -> Signal b -> Signal (a, b)
zip = Zip

fst :: Typeable a => Signal (a, b) -> Signal a
fst = Fst

snd :: Typeable b => Signal (a, b) -> Signal b
snd = Snd

--------------------------------------------------------------------------------
-- ** Instances

instance (Show a, Typeable a, Num a) => Num (Sig (Expr a))
  where
    fromInteger = repeat . fromInteger
    (+)         = zipWith (+)
    (*)         = zipWith (*)
    (-)         = zipWith (-)

    abs = todo; signum = todo;

instance (Show a, Typeable a, Fractional a) => Fractional (Sig (Expr a))
  where
    fromRational = repeat . fromRational
    (/)          = zipWith (/)

    recip = todo;

instance (Show a, Typeable a, Floating a) => Floating (Sig (Expr a))
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

instance (Typeable (Internal a), Typeable (Internal b), StructS a, StructS b)
    => StructS (a, b)
  where
    type Internal (a, b) = (Internal a, Internal b)

    fromS (a, b) = zip (fromS a) (fromS b)
    toS p        = (toS (fst p), toS (snd p))

-- --------------------------------------------------------------------------------
-- --

data Struct a
  where
    Leaf :: a        -> Struct (Leaf a)
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

instance (StructE a, StructE b) => StructE (a, b)
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
lift f = toS . mapSig (toE . f . fromE) . fromS
