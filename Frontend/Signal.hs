{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Frontend.Signal where

import Core
import Data.Dynamic
import Frontend.Stream (Str)
import Prelude ( ($), (.), id
               , Num, (+), (-), (*), fromInteger
               , Fractional, (/), fromRational
               , Floating, (**), pi, sin
               , Eq, Show, String)

import qualified Prelude         as P
import qualified Frontend.Stream as Str

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | 0-tuple value
data Empty a deriving Typeable

-- | ...
data Signal exp a
  where
    -- ^ lifts consant streams into signals
    Const :: P exp a
          => Str exp a -> Signal exp (Empty (exp a))

    -- ^ lifts stream transformers into signal transformers, possibly state-full
    Lift  :: (P exp a, P exp b)
          => (Str exp a -> Str exp b) -> (Signal exp (Empty (exp a)) -> Signal exp (Empty (exp b)))

    -- ^ maps a function over nested tuples to a function over signals
    Map   :: (Typeable a, Typeable b, StructT a, StructT b, DomainT a ~ exp, DomainT b ~ DomainT a)
          => (Struct exp a -> Struct exp b) -> Signal exp a -> Signal exp b

    -- ^ joins together two nodes
    Zip   :: (Typeable a, Typeable b, StructT a, StructT b, DomainT a ~ exp, DomainT b ~ DomainT a)
          => Signal exp a -> Signal exp b -> Signal exp (a, b)

    -- ^ breaks apart a signal of pairs, returning the first
    Fst   :: (Typeable a, Typeable b, StructT a, StructT b, DomainT a ~ exp, DomainT b ~ DomainT a)
          => Signal exp (a, b) -> Signal exp a

    -- ^ breaks apart a signal of pairs, returning the second
    Snd   :: (Typeable a, Typeable b, StructT a, StructT b, DomainT a ~ exp, DomainT b ~ DomainT a)
           => Signal exp (a, b) -> Signal exp b

    -- ^ prepends a value to the input signal
    Delay :: P exp a => exp a -> Signal exp (Empty (exp a)) -> Signal exp (Empty (exp a))

    -- ^ dummy argument used in observable sharing
    SVar  :: (Typeable a, StructT a, DomainT a ~ exp)
          => Dynamic -> Signal exp a

  deriving Typeable

-- | Signals with values ranging over the expression language
newtype Sig exp a = Sig {unSig :: Signal exp (Empty (exp a))}

--------------------------------------------------------------------------------
-- ** Instances

instance (Typeable exp, P exp a, Num (exp a)) => Num (Sig exp a)
  where
    fromInteger = repeat . fromInteger
    (+)         = zipWith (+)
    (*)         = zipWith (*)
    (-)         = zipWith (-)

    abs = todo; signum = todo;

instance (Typeable exp, P exp a, Fractional (exp a)) => Fractional (Sig exp a)
  where
    fromRational = repeat . fromRational
    (/)          = zipWith (/)

    recip = todo;

instance (Typeable exp, P exp a, Floating (exp a)) => Floating (Sig exp a)
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
-- ** "Smart" constructors

constS :: P exp a => Str exp a -> Sig exp a
constS = Sig . Const

liftS :: (P exp a, P exp b) => (Str exp a -> Str exp b) -> Sig exp a -> Sig exp b
liftS f = Sig . Lift f . unSig

mapS :: (Typeable a, Typeable b, StructT a, StructT b, DomainT a ~ exp, DomainT b ~ DomainT a)
     => (Struct exp a -> Struct exp b) -> Signal exp a -> Signal exp b
mapS = Map

delay :: P exp a => exp a -> Sig exp a -> Sig exp a
delay e = Sig . Delay e . unSig

--------------------------------------------------------------------------------
-- ** User Interface

repeat :: P exp a => exp a -> Sig exp a
repeat = constS . Str.repeat

map :: (P exp a, P exp b) => (exp a -> exp b) -> Sig exp a -> Sig exp b
map f = liftS $ Str.map f

zipWith :: (Typeable exp, P exp a, P exp b, P exp c)
        => (exp a -> exp b -> exp c)
        -> Sig exp a -> Sig exp b -> Sig exp c
zipWith f = P.curry $ lift $ P.uncurry f

--------------------------------------------------------------------------------
-- * Generalised lifting of Signals
--------------------------------------------------------------------------------

-- | Representation of nested tuples as a binary tree
data Struct exp a
  where
    Leaf :: P exp a => exp a -> Struct exp (Empty (exp a))
    Pair :: Struct exp a -> Struct exp b -> Struct exp (a, b)

-- | Similar to `Struct`, with id's at the leafs
data TStruct exp a
  where
    TLeaf :: P exp a => String -> TStruct exp (Empty (exp a))
    TPair :: TStruct exp a -> TStruct exp b -> TStruct exp (a, b)

--------------------------------------------------------------------------------

pair     :: TStruct exp a -> TStruct exp b -> TStruct exp (a, b)
pair l r = TPair l r

leaf     :: P exp a => String -> TStruct exp (Empty (exp a))
leaf s   = TLeaf s

--------------------------------------------------------------------------------

tleft  :: TStruct exp (a, b) -> TStruct exp a
tleft  t = case t of (TPair l _) -> l

tright :: TStruct exp (a, b) -> TStruct exp b
tright t = case t of (TPair _ r) -> r

tid    :: TStruct exp (Empty (exp a)) -> String
tid    t = case t of (TLeaf i) -> i

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

instance StructS (Sig exp a)
  where
    type Internal (Sig exp a) = Empty (exp a)
    type Domain   (Sig exp a) = exp

    fromS = unSig
    toS   = Sig

instance ( Typeable (Internal a)
         , Typeable (Internal b)
         , StructS a, StructT (Internal a)
         , StructS b, StructT (Internal b)
         , Domain a ~ Domain b
         , DomainT (Internal a) ~ DomainT (Internal b)
         , DomainT (Internal a) ~ Domain a
         ) =>
    StructS (a, b)
  where
    type Internal (a, b) = (Internal a, Internal b)
    type Domain   (a, b) = Domain a

    fromS (a, b) = Zip (fromS a) (fromS b)
    toS    p     = (toS (Fst p), toS (Snd p))

--------------------------------------------------------------------------------
-- ** Conversion between signals and empty structs (used to remove structs later on)

class StructT a
  where
    type DomainT a :: * -> *

    rep :: c (DomainT a) a -> TStruct (DomainT a) a

instance P exp a => StructT (Empty (exp a))
  where
    type DomainT (Empty (exp a)) = exp

    rep _ = TLeaf ""

instance ( StructT a, Typeable a
         , StructT b, Typeable b
         , DomainT a ~ DomainT b) =>
    StructT (a, b)
  where
    type DomainT (a, b) = DomainT a

    rep p = TPair (rep $ left p) (rep $ right p)
      where
        left  :: c (DomainT a) (a, b) -> c (DomainT a) a
        left  = P.undefined

        right :: c (DomainT b) (a, b) -> c (DomainT b) b
        right = P.undefined

--------------------------------------------------------------------------------
-- ** Conversion between struct's and tuples

-- | ...
class StructE a
  where
    type Normal  a :: *
    type DomainE a :: * -> *

    fromE :: Struct (DomainE a) a -> Normal a
    toE   :: Normal a -> Struct (DomainE a) a

instance P exp a => StructE (Empty (exp a))
  where
    type Normal  (Empty (exp a)) = exp a
    type DomainE (Empty (exp a)) = exp

    fromE (Leaf a) = a
    toE a          = Leaf a

instance ( StructE a
         , StructE b
         , DomainE a ~ DomainE b
         ) =>
    StructE (a, b)
  where
    type Normal  (a, b) = (Normal a, Normal b)
    type DomainE (a, b) = DomainE a

    fromE (Pair a b) = (fromE a, fromE b)
    toE   (a, b)     = Pair (toE a) (toE b)

--------------------------------------------------------------------------------
-- ** Lifting operator

-- | ...
lift
  :: (-- P (Domain s1) (Internal s1)
      --, P (Domain s2) (Internal s2)

       -- ...
       StructT (Internal s1) , StructT (Internal s2)
     , DomainT (Internal s1) ~ Domain s2
     , DomainT (Internal s2) ~ Domain s2

       -- we must be able to do the signal \ tuple transformations
     , StructS s1           , StructS s2
     , StructE (Internal s1), StructE (Internal s2)

       -- the `exp` type of the signals and tuples should be the same
     , Domain s1 ~ Domain s2
     , DomainE (Internal s1) ~ Domain s1
     , DomainE (Internal s2) ~ Domain s2

       -- requires typeable since we make use of `Zip` to transform signals
     , Typeable (Internal s1), Typeable (Internal s2)
     )
  => (Normal (Internal s1) -> Normal (Internal s2)) -> s1 -> s2
lift f = toS . mapS (toE . f . fromE) . fromS
