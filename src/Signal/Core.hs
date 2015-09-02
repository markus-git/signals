{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Signal.Core
  ( Sig(..)
  , Signal(..)
  , Symbol(..)
  , S(..)
  , U
    
  , variable
  , delay

  , Wit(..)
  , Witness(..)

  , lift
  , lift0
  , lift1
  , lift2
  ) where

import Control.Monad.Operational.Compositional hiding (join)
import Language.Embedded.VHDL (PredicateExp)

import Signal.Core.Stream (Stream(..))
import qualified Signal.Core.Stream as S

import Data.Functor.Identity
import Data.Typeable (Typeable)
import Data.Dynamic  (Dynamic)
import Data.Ref
import Data.Unique

import Prelude hiding (Left, Right, map, repeat)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Signals
--------------------------------------------------------------------------------

newtype Sig    i a = Sig    (Signal i (Identity a))

newtype Signal i a = Signal (Symbol i a)

newtype Symbol i a = Symbol (Ref (S Symbol i a))

data S sig i a
  where
    Repeat :: (Typeable a, PredicateExp (IExp i) a)
           => Stream i (IExp i a) -> S sig i (Identity a)
              
    Map    :: ( Witness i a, Typeable a
              , Witness i b, Typeable b
              )
           => (Stream i (U i a) -> Stream i (U i b))
           -> sig i a -> S sig i b

    Join   :: ( Witness i a, Typeable a
              , Witness i b, Typeable b
              )
           => sig i a -> sig i b -> S sig i (a, b)
    
    Left   :: ( Witness i a, Typeable a
              , Witness i b, Typeable b
              )
           => sig i (a, b) -> S sig i a
    
    Right  :: ( Witness i a, Typeable a
              , Witness i b, Typeable b
              )
           => sig i (a, b) -> S sig i b

    Delay  :: (Typeable a, PredicateExp (IExp i) a)
           => IExp i a -> sig i (Identity a) -> S sig i (Identity a)

    Var    :: (Witness i a, Typeable a)
           => Dynamic -> S sig i a

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
-- ** 

repeat
  :: (Typeable a, PredicateExp (IExp i) a)
  => Stream i (U i (Identity a))
  -> Signal i (Identity a)
repeat s = signal $ Repeat s

map
  :: ( Witness i a, Typeable a
     , Witness i b, Typeable b
     )
  => (Stream i (U i a) -> Stream i (U i b))
  -> Signal i a
  -> Signal i b
map f (Signal s) = signal $ Map f s

join
  :: ( Witness i a, Typeable a
     , Witness i b, Typeable b
     )
  => Signal i a
  -> Signal i b
  -> Signal i (a, b)
join (Signal a) (Signal b) = signal $ Join a b

left
  :: ( Witness i a, Typeable a
     , Witness i b, Typeable b
     )
  => Signal i (a, b)
  -> Signal i a
left (Signal s) = signal $ Left s

right
  :: ( Witness i a, Typeable a
     , Witness i b, Typeable b
     )
  => Signal i (a, b)
  -> Signal i b
right (Signal s) = signal $ Right s

variable
  :: (Witness i a, Typeable a)
  => Dynamic
  -> Signal i a
variable = signal . Var 

--------------------------------------------------------------------------------
-- **

delay :: (Typeable a, PredicateExp e a, e ~ IExp i) => e a -> Sig i a -> Sig i a
delay e (Sig (Signal s)) = Sig . signal $ Delay e s

--------------------------------------------------------------------------------
-- ** Properties of signals

instance Eq (Signal i a) where
  Signal (Symbol s1) == Signal (Symbol s2) = s1 == s2

instance (Num (IExp i a), PredicateExp (IExp i) a, Typeable a) => Num (Sig i a) where
  (+)         = lift2 (+)
  (-)         = lift2 (-)
  (*)         = lift2 (*)
  negate      = lift1 negate
  abs         = lift1 abs
  signum      = lift1 signum
  fromInteger = lift0 . fromInteger

instance (Fractional (IExp i a), PredicateExp (IExp i) a, Typeable a) => Fractional (Sig i a) where
  (/)          = lift2 (/)
  recip        = lift1 recip
  fromRational = lift0 . fromRational

--------------------------------------------------------------------------------
-- * Nested Signals
--------------------------------------------------------------------------------

data Wit i a
  where
    WE :: ( Typeable a
          , PredicateExp (IExp i) a
          )
       => Wit i (Identity a)
    
    WP :: ( Witness i a, Typeable a
          , Witness i b, Typeable b
          )
       => Wit i a -> Wit i b -> Wit i (a, b)

class Witness i a
  where
    wit :: Wit i a

instance
    ( Typeable a
    , PredicateExp (IExp i) a
    )
    => Witness i (Identity a)
  where
    wit = WE

instance forall i a b.
    ( Witness i a, Typeable a
    , Witness i b, Typeable b
    )
    => Witness i (a, b)
  where
    wit = WP (wit :: Wit i a) (wit :: Wit i b)

--------------------------------------------------------------------------------
-- ** Type for nested tuples of signals

type family Packed (i :: (* -> *) -> * -> *) a :: *
type instance Packed i (Identity a) = Sig i a
type instance Packed i (a, b)       = (Packed i a, Packed i b)

pack :: forall i a. Witness i a => Signal i a -> Packed i a
pack s = go (wit :: Wit i a) s
  where
    go :: Wit i x -> Signal i x -> Packed i x
    go (WE)     s = Sig s
    go (WP u v) s = (,) (go u $ left s) (go v $ right s)

unpack :: forall i a. Witness i a => Packed i a -> Signal i a
unpack s = go (wit :: Wit i a) s
  where
    go :: Wit i x -> Packed i x -> Signal i x
    go (WE)     (Sig s) = s
    go (WP u v) (l, r)  = join (go u l) (go v r)

--------------------------------------------------------------------------------
-- ** General lifting operator

-- todo: I don't like the proxy, or the type family. Possible to find U^(-1)?
lift :: forall proxy i e a b.
        ( Witness i a, Typeable a
        , Witness i b, Typeable b
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

lift0
  :: ( Typeable a, PredicateExp e a
     , e ~ IExp i
     )
  => e a
  -> Sig i a
lift0 = Sig . repeat . S.repeat

lift1
  :: forall i e a b.
     ( Typeable a, PredicateExp e a
     , Typeable b, PredicateExp e b
     , e ~ IExp i
     )
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
     , e ~ IExp i
     )
  => (e a -> e b -> e c)
  -> Sig i a
  -> Sig i b
  -> Sig i c
lift2 f = curry $ lift p $ uncurry f
  where
    p = undefined :: proxy i (Identity a, Identity b) (Identity c)

--------------------------------------------------------------------------------
-- the end.
