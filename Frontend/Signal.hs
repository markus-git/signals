{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Frontend.Signal where

import Interpretation

import           Frontend.Stream (Stream, Str)
import qualified Frontend.Stream as S

import           Prelude ( ($), (.), id
                         , Num, (+), (-), (*), fromInteger
                         , Fractional, (/), fromRational
                         , Floating, (**), pi, sin
                         , Eq, Show, String)
import qualified Prelude as P

import Data.Dynamic
import Data.Typeable

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | ...
data Signal exp a
  where
    -- ^ lifts consant streams into signals
    Const :: Typeable a => Stream exp (exp a) -> Signal exp (Empty (exp a))

    -- ^ lifts stream transformers into signal transformers, possibly state-full
    Lift  :: (Typeable a, Typeable b)
          => (Stream exp (exp a)         -> Stream exp (exp b))
          -> (Signal exp (Empty (exp a)) -> Signal exp (Empty (exp b)))

    -- ^ maps a function over nested tuples to a function over signals
    Map   :: (Typeable a, Typeable b)
          => (Struct a -> Struct b) -> Signal exp a -> Signal exp b

    -- ^ joins together two nodes
    Zip   :: (Typeable a, Typeable b)
          => Signal exp a -> Signal exp b -> Signal exp (a, b)

    -- ^ breaks apart a signal of pairs, returning the first
    Fst   :: Typeable a => Signal exp (a, b) -> Signal exp a

    -- ^ breaks apart a signal of pairs, returning the second
    Snd   :: Typeable b => Signal exp (a, b) -> Signal exp b

    -- ^ prepends a value to the input signal
    Delay :: Typeable a
          => exp a -> Signal exp (Empty (exp a)) -> Signal exp (Empty (exp a))

    -- ^ dummy argument used in observable sharing
    SVar  :: Typeable a => Dynamic -> Signal exp a

  deriving (Typeable)

-- | ...
newtype Sig exp a = Sig {unSig :: Signal exp (Empty (exp a))}

--------------------------------------------------------------------------------
-- ** Instances

class (Typeable (exp :: * -> *), Typeable (a :: *)) => Typ exp a

instance (Typ exp a, Num (exp a), Eq (exp a)) => Num (Sig exp a)
  where
    fromInteger = repeat . fromInteger
    (+)         = zipWith (+)
    (*)         = zipWith (*)
    (-)         = zipWith (-)

    abs = todo; signum = todo;

instance (Typ exp a, Fractional (exp a), Eq (exp a)) => Fractional (Sig exp a)
  where
    fromRational = repeat . fromRational
    (/)          = zipWith (/)

    recip = todo;

instance (Typ exp a, Floating (exp a), Eq (exp a)) => Floating (Sig exp a)
  where
    pi   = repeat pi
    sin  = map sin
    (**) = zipWith (**)

    exp   = todo; sqrt  = todo; log     = todo;
    tan   = todo; cos   = todo; asin    = todo;
    atan  = todo; acos  = todo; sinh    = todo;
    tanh  = todo; cosh  = todo; asinh   = todo;
    atanh = todo; acosh = todo; logBase = todo;

todo = P.error "unsupported operation"

--------------------------------------------------------------------------------
-- ** ...

constS :: (Typeable a) => Str exp a -> Sig exp a
constS = Sig . Const

liftS :: (Typeable a, Typeable b)
      => (Str exp a -> Str exp b) -> Sig exp a -> Sig exp b
liftS f = Sig . Lift f . unSig

mapS :: (Typeable a, Typeable b)
     => (Struct a -> Struct b) -> Signal exp a -> Signal exp b
mapS = Map

--------------------------------------------------------------------------------
-- ** User Interface
--------------------------------------------------------------------------------

repeat :: (Typeable a) => exp a -> Sig exp a
repeat = constS . S.repeat

map :: (Typeable a, Typeable b) => (exp a -> exp b) -> Sig exp a -> Sig exp b
map f = liftS $ S.map f

delay :: (Typeable a) => exp a -> Sig exp a -> Sig exp a
delay e = Sig . Delay e . unSig

zipWith :: (Typeable exp, Typeable a, Typeable b, Typeable c)
        => (exp a -> exp b -> exp c)
        -> Sig exp a -> Sig exp b -> Sig exp c
zipWith f = P.curry $ lift $ P.uncurry f

--------------------------------------------------------------------------------
-- * Generalised lifting of Signals
--------------------------------------------------------------------------------

-- | 0-tuple value
data Empty a deriving Typeable

-- | Representation of nested tuples as a binary tree
data Struct a
  where
    Leaf :: exp a -> Struct (Empty (exp a))
    Pair :: Struct a -> Struct b -> Struct (a, b)
  deriving
    Typeable

-- | Similar to `Struct`, with id's at the leafs
data TStruct a
  where
    TLeaf :: String -> TStruct (Empty (exp a))
    TPair :: TStruct a -> TStruct b -> TStruct (a, b)
  deriving
    Typeable

--------------------------------------------------------------------------------
-- ** Conversion between signals and tuples

-- | ...
class StructS a
  where
    type Internal a :: *
    type Domain   a :: * -> *

    fromS :: a -> Signal (Domain a) (Internal a)
    toS   :: Signal (Domain a) (Internal a) -> a

instance StructS (Signal exp (Empty (exp a)))
  where
    type Internal (Signal exp (Empty (exp a))) = Empty (exp a)
    type Domain   (Signal exp (Empty (exp a))) = exp

    fromS = id
    toS   = id

instance ( Typeable (Internal a)
         , Typeable (Internal b)
         , StructS a
         , StructS b
         , Domain a ~ Domain b
         )
    => StructS (a, b)
  where
    type Internal (a, b) = (Internal a, Internal b)
    type Domain   (a, b) = Domain a

    fromS (a, b) = Zip (fromS a) (fromS b)
    toS    p     = (toS (Fst p), toS (Snd p))

instance StructS (Sig exp a)
  where
    type Internal (Sig exp a) = Empty (exp a)
    type Domain   (Sig exp a) = exp

    fromS = unSig
    toS   = Sig

--------------------------------------------------------------------------------
-- ** Conversion between signals and empty structs

class StructT a
  where
    type TInternal a :: *

    repS :: a -> TStruct (TInternal a)

instance StructT (Signal exp (Empty (exp a)))
  where
    type TInternal (Signal exp (Empty (exp a))) = Empty (exp a)

    repS _ = TLeaf ""

instance (StructT a, StructT b) => StructT (a, b)
  where
    type TInternal (a, b) = (TInternal a, TInternal b)

    repS (a, b)  = TPair (repS a) (repS b)

instance StructT (Sig exp a)
  where
    type TInternal (Sig exp a) = Empty (exp a)

    repS = repS . unSig

--------------------------------------------------------------------------------
-- ** Conversion between struct's and tuples

-- | ...
class StructE a
  where
    type Normal a :: *

    fromE :: Struct a -> Normal a
    toE   :: Normal a -> Struct a

instance StructE (Empty (exp a))
  where
    type Normal (Empty (exp a)) = exp a

    fromE (Leaf a) = a
    toE a          = Leaf a

instance (StructE a, StructE b) => StructE (a, b)
  where
    type Normal (a, b) = (Normal a, Normal b)

    fromE (Pair a b) = (fromE a, fromE b)
    toE   (a, b)     = Pair (toE a) (toE b)

--------------------------------------------------------------------------------
-- ** Lifting operator

lift
  :: ( StructS s1            , StructS s2
     , StructE (Internal s1) , StructE (Internal s2)
     , Typeable (Internal s1), Typeable (Internal s2)
     , Domain s1 ~ Domain s2
     )
  => (Normal (Internal s1) -> Normal (Internal s2)) -> s1 -> s2
lift f = toS . mapS (toE . f . fromE) . fromS
