{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Signal where

import Core hiding (Ref, join)
import Frontend.Stream (Stream(..))

import Data.Functor.Identity
import Data.Ref
import Data.Unique

import Prelude hiding (Left, Right, map, repeat)

import qualified Frontend.Stream as S
import qualified Prelude         as P

--------------------------------------------------------------------------------
-- * Signals
--------------------------------------------------------------------------------

newtype Sig    i a = Sig    (Signal i (Identity a))

newtype Signal i a = Signal (Symbol i a)

newtype Symbol i a = Symbol (Ref (S Symbol i a))

data S sig i a
  where
    Repeat :: Stream i (IExp i a)
           -> S sig i (Identity a)
    Map    :: (Witness a, Witness b)
           => (Stream i (U i a) -> Stream i (U i b))
           ->   sig i a
           -> S sig i b

    Join   :: (Witness a, Witness b) => sig i a      -> sig i b -> S sig i (a, b)
    Left   :: (Witness a, Witness b) => sig i (a, b)            -> S sig i a
    Right  :: (Witness a, Witness b) => sig i (a, b)            -> S sig i b

    Delay  :: IExp i a -> sig i (Identity a) -> S sig i (Identity a)

-- | Type for nested tuples, stripping away all Identity
type family U (i :: (* -> *) -> * -> *) a :: *
type instance U i (Identity a) = IExp i a
type instance U i (a, b)       = (U i a, U i b)

--------------------------------------------------------------------------------
-- ** Some `smart` constructions

signal :: S Symbol i a -> Signal i a
signal = Signal . symbol

symbol :: S Symbol i a -> Symbol i a
symbol = Symbol . ref

unsymbol :: Symbol i a -> S Symbol i a
unsymbol (Symbol s) = deref s

--------------------------------------------------------------------------------
-- ** Combinatorial

repeat :: Stream i (U i (Identity a)) -> Signal i (Identity a)
repeat s = signal $ Repeat s

map :: (Witness a, Witness b) =>
          (Stream i (U i a) -> Stream i (U i b))
       -> Signal i a
       -> Signal i b
map f (Signal s) = signal $ Map f s

join :: (Witness a, Witness b) =>
           Signal i a
        -> Signal i b
        -> Signal i (a, b)
join (Signal a) (Signal b) = signal $ Join a b

left :: (Witness a, Witness b) =>
           Signal i (a, b)
        -> Signal i a
left (Signal s) = signal $ Left s

right :: (Witness a, Witness b) =>
            Signal i (a, b)
         -> Signal i b
right (Signal s) = signal $ Right s

--------------------------------------------------------------------------------
-- ** Sequential

delay :: (e ~ IExp i) => e a -> Sig i a -> Sig i a
delay e (Sig (Signal s)) = Sig . signal $ Delay e s

--------------------------------------------------------------------------------
-- ** Properties of signals

instance Eq (Signal i a) where
  Signal (Symbol s1) == Signal (Symbol s2) = s1 == s2
{-
instance Num (IExp i a) => Num (Signal i a)
  where
    (+)         = undefined
    (-)         = undefined
    (*)         = undefined
    negate      = undefined
    abs         = undefined
    signum      = undefined
    fromInteger = undefined
-}
--------------------------------------------------------------------------------
-- * Nested Signals
--------------------------------------------------------------------------------

data Wit a
  where
    WE :: Wit (Identity a)
    WP :: ( Witness a
          , Witness b
          )
       => Wit a
       -> Wit b
       -> Wit (a, b)

class Witness a
  where
    wit :: Wit a

instance Witness (Identity a)
  where
    wit = WE

instance forall a b.
    ( Witness a
    , Witness b
    )
    => Witness (a, b)
  where
    wit = WP (wit :: Wit a) (wit :: Wit b)

--------------------------------------------------------------------------------
-- ** Type for nested tuples of signals

type family Packed (i :: (* -> *) -> * -> *) a :: *
type instance Packed i (Identity a) = Sig i a
type instance Packed i (a, b)       = (Packed i a, Packed i b)

pack :: forall i a. Witness a => Signal i a -> Packed i a
pack s = go (wit :: Wit a) s
  where
    go :: Wit x -> Signal i x -> Packed i x
    go (WE)     s = Sig s
    go (WP u v) s = (,) (go u $ left s) (go v $ right s)

unpack :: forall i a. Witness a => Packed i a -> Signal i a
unpack s = go (wit :: Wit a) s
  where
    go :: Wit x -> Packed i x -> Signal i x
    go (WE)     (Sig s) = s
    go (WP u v) (l, r)  = join (go u l) (go v r)

--------------------------------------------------------------------------------
-- ** General lifting operator

-- todo: I don't like the proxy, or the type family. Possible to find U^(-1)?
lift :: forall proxy i e a b.
        ( Witness a
        , Witness b
        , e ~ IExp i
        )
     => proxy i a b
     -> (U i a      -> U i b)
     -> (Packed i a -> Packed i b)
lift _ f = pack . h . unpack
  where
    g = stream f       :: Stream i (U i a) -> Stream i (U i b)
    h = map (stream f) :: Signal i a       -> Signal i b

stream :: (a -> b) -> (Stream i a -> Stream i b)
stream f (Stream s) = Stream $ fmap (fmap f) s

--------------------------------------------------------------------------------
-- ** Some common lifting operations

lift0 :: (e ~ IExp i) => e a -> Sig i a
lift0 = Sig . repeat . S.repeat

lift1 :: forall i e a b. (e ~ IExp i) => (e a -> e b) -> Sig i a -> Sig i b
lift1 f = lift p f
  where
    p = undefined :: proxy i (Identity a) (Identity b)

lift2 :: forall i e a b c. (e ~ IExp i) => (e a -> e b -> e c) -> Sig i a -> Sig i b -> Sig i c
lift2 f = curry $ lift p $ uncurry f
  where
    p = undefined :: proxy i (Identity a, Identity b) (Identity c)

--------------------------------------------------------------------------------
-- the end.
