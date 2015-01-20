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

import Frontend.Signal ( Signal(..), Sig(..)
                       , StructT(..)
                       , Empty, Struct, TStruct)
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

    TVar    :: TSignal exp r

    -- ^ Signal
    TConst  :: (Typeable a, Typeable exp) => Stream exp (exp a) -> TSignal exp r

    TLift   :: (Typeable a, Typeable b)
            => (Stream exp (exp a) -> Stream exp (exp b)) -> r -> TSignal exp r

    TMap    :: (Typeable a, Typeable b)
            => TStruct exp a -> (Struct exp a -> Struct exp b) -> r -> TSignal exp r

    TZip    :: TStruct exp a -> TStruct exp b -> r -> r -> TSignal exp r

    TFst    :: TStruct exp a -> r -> TSignal exp r

    TSnd    :: TStruct exp a -> r -> TSignal exp r

    TDelay  :: (Typeable a) => exp a -> r -> TSignal exp r

    -- ^ Buffers
    TVBuff  :: r ->            TSignal exp r
    TDBuff  :: r -> exp Int -> TSignal exp r

  deriving (Typeable)

--------------------------------------------------------------------------------
-- ** MuRef instances for signals

instance (Typeable exp) => MuRef (Signal exp a)
  where
    type DeRef (Signal exp a) = TSignal exp

    mapDeRef f node = case node of
      (Const sf)   -> pure $ TConst sf
      (Lift  sf s) -> TLift sf <$> f s

--      (Map   sf s) -> TMap (rep s) sf <$> f s

--      (Zip   s  u) -> TZip (rep s) (rep u) <$> f s <*> f u

--      (Fst   s)    -> TFst undefined <$> f s
--      (Snd   s)    -> TSnd undefined <$> f s

--      (Delay a s)  -> TDelay a <$> f s
--      (SVar  _)    -> pure $ TVar

instance (Typeable a, Typeable b, Typeable exp) =>
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
-- **

-- how do I prove that, forall a. DomainT a ~ exp?

data Wt c
  where
    Wt :: c => Wt c

wt :: Signal exp a -> Wt (DomainT a ~ exp)
wt (Const  _) = Wt
wt (Lift _ _) = Wt
wt x@(Map  _ s)
  | Wt <- wt s
  , Wt <- wd s x
  = Wt
wt (Zip l r)
  | Wt <- wt l
  , Wt <- wt r
  = Wt
wt (Fst s)
  | Wt <- wt s
  = Wt
wt x@(Snd s)
  | Wt <- wt s
  , Wt <- wd s x
  = Wt
wt (Delay _ s)
  | Wt <- wt s
  = Wt

wd :: Signal exp a -> Signal exp b -> Wt (DomainT a ~ DomainT b)
wd s u
  | Wt <- wt s
  , Wt <- wt u
  = Wt

--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

instance Show (TSignal exp Unique) where
  show node = case node of
    (TLambda i b)  -> "lam. "   ++ show i ++ " " ++ show b
    (TVar)         -> "var. "
    (TConst _)     -> "const. "
    (TLift  _ s)   -> "lift. "  ++ show s
    (TMap _ _ s)   -> "map. "   ++ show s
    (TZip _ _ s u) -> "zip. "   ++ show s ++ " " ++ show u
    (TFst _ s)     -> "fst. "   ++ show s
    (TSnd _ s)     -> "snd. "   ++ show s
    (TDelay _ s)   -> "delay. " ++ show s
    (TVBuff r)     -> "vbuff ." ++ show r
    (TDBuff r _)   -> "dbuff ." ++ show r
