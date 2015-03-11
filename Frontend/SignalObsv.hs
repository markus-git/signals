{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Frontend.SignalObsv where

import Interpretation

import Frontend.Signal (Signal(..), Sig(..), StructT(..), Empty, Struct, TStruct)
import Frontend.Stream (Stream(..), Str(..))

import Control.Applicative hiding (Const)
import Data.Dynamic
import Data.Proxy
import Data.Reify
import Data.Typeable

--------------------------------------------------------------------------------
-- * Graph representation of Signals
--------------------------------------------------------------------------------

data TSignal exp r
  where
    -- ^ Signal functions
    TLambda :: r -> r -> TSignal exp r

    TVar    :: (Typeable a, Typeable exp)
            => TStruct exp a
            -> TSignal exp r

    -- ^ Signal
    TConst  :: (Typeable a, Typeable exp)
            => Stream exp (exp a) -> TSignal exp r

    TLift   :: (Typeable a, Typeable b, Typeable exp)
            => (Stream exp (exp a) -> Stream exp (exp b))
            -> r -> TSignal exp r

    TMap    :: (Typeable a, Typeable b, Typeable exp)
            => TStruct exp a -> TStruct exp b
            -> (Struct exp a -> Struct exp b)
            -> r -> TSignal exp r

    TZip    :: (Typeable a, Typeable b)
            => TStruct exp a
            -> TStruct exp b
            -> r -> r -> TSignal exp r

    TFst    :: (Typeable a, Typeable b)
            => TStruct exp (a, b)
            -> r -> TSignal exp r

    TSnd    :: (Typeable a, Typeable b)
            => TStruct exp (a, b)
            -> r -> TSignal exp r

    TDelay  :: (Typeable a, Typeable exp) => exp a -> r -> TSignal exp r

    -- ^ Buffers
    TBuff   :: (Typeable a, Typeable exp)
            => proxy (exp a)
            -> r -> TSignal exp r

  deriving (Typeable)

type Node e = TSignal e Unique

--------------------------------------------------------------------------------
-- ** Helper functions

edges :: TSignal e a -> [a]
edges node =
  case node of
    TLambda x y  -> [x, y]
    TVar _       -> []
    TConst _     -> []
    TLift  _ x   -> [x]
    TMap _ _ _ x -> [x]
    TZip _ _ x y -> [x, y]
    TFst _ x     -> [x]
    TSnd _ x     -> [x]
    TDelay _ x   -> [x]

--------------------------------------------------------------------------------
-- ** MuRef instances for signals

instance (Typeable exp) => MuRef (Signal exp a)
  where
    type DeRef (Signal exp a) = TSignal exp

    mapDeRef f node = case node of
      (Const sf)   -> pure $ TConst sf
      (Lift  sf s) -> TLift sf <$> f s
      (Map   sf s) -> TMap (rep s) (rep (undefined :: Struct exp a)) sf <$> f s
      (Zip   s  u) -> TZip (rep s) (rep u) <$> f s <*> f u
      (Fst   s)    -> TFst (rep s) <$> f s
      (Snd   s)    -> TSnd (rep s) <$> f s
      (Delay a s)  -> TDelay a <$> f s
      (SVar  _)    -> pure $ TVar (rep (undefined :: Struct exp a))

instance ( Typeable a, Typeable b, Typeable exp
         , StructT a,  DomainT a ~ exp
         ) =>
    MuRef (Signal exp a -> Signal exp b)
  where
    type DeRef (Signal exp a -> Signal exp b) = TSignal exp

    mapDeRef f sf =
      let (v, sg) = let a = SVar (toDyn sf) in (a, sf a)
       in TLambda <$> f v <*> f sg

--------------------------------------------------------------------------------
-- ** MuRef instances for sig

instance (Typeable exp) => MuRef (Sig exp a)
  where
    type DeRef (Sig exp a) = TSignal exp

    mapDeRef f node = mapDeRef f (unSig node)

instance (Typeable a, Typeable b, Typeable exp) =>
    MuRef (Sig exp a -> Sig exp b)
  where
    type DeRef (Sig exp a -> Sig exp b) = TSignal exp

    mapDeRef f sf = mapDeRef f (unSig . sf . Sig)

--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

instance Show a => Show (TSignal exp a) where
  show node = case node of
    (TLambda i b)  -> "lam. "   ++ show i ++ " " ++ show b
    (TVar _)       -> "var. "
    (TConst _)     -> "const. "
    (TLift  _ s)   -> "lift. "  ++ show s
    (TMap _ _ _ s) -> "map. "   ++ show s
    (TZip _ _ s u) -> "zip. "   ++ show s ++ " " ++ show u
    (TFst _ s)     -> "fst. "   ++ show s
    (TSnd _ s)     -> "snd. "   ++ show s
    (TDelay _ s)   -> "delay. " ++ show s
    (TBuff _ r)    -> "dbuff ." ++ show r
