{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Signal where

import Core
import Backend.Nested

import           Frontend.Stream (Stream, Str)
import qualified Frontend.Stream as S

import Data.Dynamic (Dynamic)
import Prelude ( Eq, Show, String, ($), (.), id 
               , Num,        (+), (-), (*), fromInteger
               , Fractional, (/), fromRational
               , Floating,   (**), pi, sin
               , curry, uncurry
               , undefined
               )

--------------------------------------------------------------------------------
-- * Signals
--------------------------------------------------------------------------------

data Signal (instr :: (* -> *) -> * -> *) (a :: *)
  where
    -- ^ ...
    Const :: Stream instr a -> Signal instr (Empty a)

    -- ^ ...
    Lift  :: (Stream instr a -> Stream instr b) -> Signal instr (Empty a) -> Signal instr (Empty b)

    -- ^ ...
    Map   :: (IExp instr ~ Domain a, Domain a ~ Domain b)
          => (Tuple (IExp instr) a -> Tuple (IExp instr) b) -> Signal instr a -> Signal instr b

    -- ^ ...
    Join  :: (IExp instr ~ Domain a, Domain a ~ Domain b, Puple a, Puple b)
          => Signal instr a -> Signal instr b -> Signal instr (a, b)

    -- ^ ...
    Left  :: (IExp instr ~ Domain a, Domain a ~ Domain b, Puple a)
          => Signal instr (a, b) -> Signal instr a

    -- ^ ...
    Right :: (IExp instr ~ Domain a, Domain a ~ Domain b, Puple b)
          => Signal instr (a, b) -> Signal instr b

    -- ^ ...
    Var   :: Dynamic -> Signal instr a

-- | `Shorthand` for signals which produce values of type `exp a`
newtype Sig instr a = Sig { unSig :: Signal instr (Empty (IExp instr a)) }

--------------------------------------------------------------------------------

repeat :: (e ~ IExp instr) => e a -> Sig instr a
repeat = Sig . Const . S.repeat

map :: (e ~ IExp instr) => (e a -> e b) -> Sig instr a -> Sig instr b
map f = Sig . Lift (S.map f) . unSig

zipWith :: (e ~ IExp instr) => (e a -> e b -> e c) -> Sig instr a -> Sig instr b -> Sig instr c
zipWith f = curry $ lift $ uncurry f

--------------------------------------------------------------------------------
-- * Nested Signals
--------------------------------------------------------------------------------

type family   Domain a :: * -> *
type instance Domain (Empty (expr a)) = expr
type instance Domain (a, b)           = Domain a

-- | Conversion between signals and tuples
class SSignal a
  where
    type Instr    a :: (* -> *) -> * -> *
    type Internal a :: *
    
    sugarS   :: a -> Signal (Instr a) (Internal a)
    desugarS :: Signal (Instr a) (Internal a) -> a

-- | Conversion betwee instructions and tuples
class STuple a
  where
    type Normal a :: *
    
    sugarE   :: Normal a -> Tuple (Domain a) a 
    desugarE :: Tuple (Domain a) a -> Normal a

--------------------------------------------------------------------------------

instance SSignal (Sig instr a)
  where
    type Instr    (Sig instr a) = instr
    type Internal (Sig instr a) = Empty (IExp instr a)

    sugarS   = unSig
    desugarS = Sig

instance SSignal (Signal instr (Empty a))
  where
    type Instr    (Signal instr (Empty a)) = instr
    type Internal (Signal instr (Empty a)) = Empty a

    sugarS   = id
    desugarS = id

instance ( SSignal a, SSignal b, Instr a ~ Instr b
         , Puple  (Internal a), Puple (Internal b)
         , Domain (Internal a) ~ IExp   (Instr a)
         , Domain (Internal a) ~ Domain (Internal b))
    => SSignal (a, b)
  where
    type Instr    (a, b) = Instr a
    type Internal (a, b) = (Internal a, Internal b)

    sugarS   (a, b) = Join (sugarS a) (sugarS b)
    desugarS s      = (desugarS (Left s), desugarS (Right s))

--------------------------------------------------------------------------------

instance STuple (Empty (expr a))
  where
    type Normal (Empty (expr a)) = expr a

    sugarE            = Leaf
    desugarE (Leaf a) = a

instance (STuple a, STuple b, Domain a ~ Domain b) => STuple (a, b)
  where
    type Normal (a, b) = (Normal a, Normal b)

    sugarE   (a, b)       = Branch (sugarE a) (sugarE b)
    desugarE (Branch a b) = (desugarE a, desugarE b)

--------------------------------------------------------------------------------
-- **

-- | ...
lift :: ( SSignal a
        , SSignal b
        , Instr a ~ Instr b
        , STuple (Internal a)
        , STuple (Internal b)
        , Domain (Internal a) ~ IExp (Instr a)
        , Domain (Internal b) ~ IExp (Instr b)
        )
     => (Normal (Internal a) -> Normal (Internal b)) -> a -> b
lift f = desugarS . Map (sugarE . f . desugarE) . sugarS


--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | ...
type family   Phantom tup
type instance Phantom (a,b)         = (Phantom a, Phantom b)
type instance Phantom (Empty (e a)) = Empty (Shantom e a)

-- | Phantom type used to have tuples over string while maintaining type information
data Shantom (c :: * -> *) (a :: *) = S String

-- | Tuples over strings
type Suple c a = Tuple (Shantom c) (Phantom a)
-- | ..
class Puple a
  where
    represent :: Tuple (Domain a) a -> Suple (Domain a) a

--------------------------------------------------------------------------------

instance Puple (Empty (expr a))
  where
    represent _ = Leaf (S "")

instance (Domain a ~ Domain b, Puple a, Puple b) => Puple (a, b)
  where
    represent (_ :: Tuple (Domain a) (a, b)) = Branch (represent left) (represent right)
      where
        left  = undefined :: Tuple (Domain a) a
        right = undefined :: Tuple (Domain b) b

--------------------------------------------------------------------------------
-- ** 

-- | ...
rep :: forall instr a. (Domain a ~ IExp instr, Puple a) => Signal instr a -> Suple (IExp instr) a
rep _ = represent (undefined :: Tuple (IExp instr) a)

