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

import Frontend.Signal (Signal(..), Sig(..), Struct(..), Empty)
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
            => TStruct a -> (Struct a -> Struct b) -> r -> TSignal exp r

    TZip    :: (Typeable a, Typeable b)
            => TStruct a -> TStruct b -> r -> r -> TSignal exp r

    TFst    :: Typeable a => TStruct a -> r -> TSignal exp r

    TSnd    :: Typeable a => TStruct a -> r -> TSignal exp r

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
{-
      (Map   sf s) -> TMap undefined  sf <$> f s
      (Zip   (s :: Signal exp l)
             (u :: Signal exp r))
                   -> TZip undefined undefined <$> f s <*> f u
      (Fst   s)    -> TFst undefined    <$> f s
      (Snd   s)    -> TSnd undefined    <$> f s
-}
      (Delay a s)  -> TDelay a <$> f s
      (SVar  _)    -> pure $ TVar

instance (Typeable a, Typeable b, Typeable exp) =>
    MuRef (Signal exp a -> Signal exp b)
  where
    type DeRef (Signal exp a -> Signal exp b) = TSignal exp

    mapDeRef f sf =
      let (v, sg) = let a = SVar (toDyn sf) in (a, sf a)
       in TLambda <$> f v <*> f sg

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

data TStruct a
  where
    TLeaf :: String                 -> TStruct (Empty (exp a))
    TPair :: TStruct a -> TStruct b -> TStruct (a, b)

ts :: Struct a -> TStruct a
ts (Leaf _)   = TLeaf ""
ts (Pair l r) = TPair (ts l) (ts r)

tsp :: forall a. Typeable a => Proxy a -> TStruct a
tsp _ | typeRepTyCon ty == pc = let [x, y] = typeRepArgs ty
                                 in TPair (tsp (Proxy :: Proxy x))
                                          (tsp (Proxy :: Proxy y))
  where
    ty :: TypeRep
    ty = typeOf (undefined :: a)

    pc :: TyCon
    pc = typeRepTyCon $ typeOf (undefined :: (a, a))

--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

instance Show (TSignal exp Unique) where
  show node = case node of
    (TLambda i b)  -> "lam. " ++ show i ++ " " ++ show b
    (TVar)         -> "var. "
    (TConst _)     -> "const. "
    (TLift  _ s)   -> "lift. " ++ show s
    (TMap _ _ s)   -> "map. " ++ show s
    (TZip _ _ s u) -> "zip. " ++ show s ++ " " ++ show u
    (TFst _ s)     -> "fst. " ++ show s
    (TSnd _ s)     -> "snd. " ++ show s
    (TDelay _ s)   -> "delay. " ++ show s
    (TVBuff r)     -> "vbuff ." ++ show r
    (TDBuff r _)   -> "dbuff ." ++ show r
