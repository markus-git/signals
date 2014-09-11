{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE FlexibleInstances    #-}

module Frontend.SignalObsv where

import Core
import Expr hiding (Var)

import Frontend.Stream (Stream)
import Frontend.Signal

import Control.Applicative hiding (Const)
import Data.Dynamic
import Data.Reify

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

data SigTree ref
  where
    -- | Signal functions
    TLambda :: ref -> ref -> SigTree ref
    TVar    ::               SigTree ref

    -- | Signals
    TConst  :: (Typeable a) => (Stream a) -> SigTree ref
    TLift   :: (Typeable a, Typeable b)
            => (Stream a -> Stream b)
            -> ref
            -> SigTree ref

    TZip    :: ref -> ref -> SigTree ref
    TFst    :: ref        -> SigTree ref

    TDelay  :: (Typeable a) => Expr a   -> ref -> SigTree ref
    TSample ::                 Expr Int -> ref -> SigTree ref

class NewVar a
  where
    mkVar :: Dynamic -> a

--------------------------------------------------------------------------------
-- **

deriving instance Typeable1 Expr
deriving instance Typeable1 Stream
deriving instance Typeable1 Signal

instance NewVar (Signal a)
  where
    mkVar = Var

instance MuRef (Signal a)
  where
    type DeRef (Signal a) = SigTree

    mapDeRef f node = case node of
      (Const sf)   -> pure $ TConst sf
      (Lift  sf s) -> TLift sf <$> f s

      (Zip s u)    -> TZip <$> f s <*> f u
      (Fst s)      -> TFst <$> f s

      (Delay  v s) -> TDelay  v <$> f s
      (Sample n s) -> TSample n <$> f s

      (Var _)      -> pure $ TVar

instance (Typeable a, Typeable b) => MuRef (Signal a -> Signal b)
  where
    type DeRef (Signal a -> Signal b) = SigTree

    mapDeRef f sf = let (v, sg) = capture sf
                     in TLambda <$> f v <*> f sg

capture :: (Typeable a, Typeable b, NewVar a) => (a -> b) -> (a, b)
capture f = let a = mkVar (toDyn f) in (a, f a)
