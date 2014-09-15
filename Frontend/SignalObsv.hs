{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Frontend.SignalObsv where

import Core
import Expr hiding (Var)

import Frontend.Stream (Stream)
import Frontend.Signal

import Control.Applicative hiding (Const)
import Data.Dynamic
import Data.Reify
import Data.Proxy

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

data SigTree ref
  where
    -- | Signal functions
    TLambda :: (Typeable a, Typeable b)
            => Proxy (a, b) -> ref -> ref -> SigTree ref
    TVar    :: SigTree ref

    -- | Signals
    TConst  :: (Typeable a) => (Stream (Expr a)) -> SigTree ref

    TLift   :: (Typeable a, Typeable b)
            => (Stream (Expr a) -> Stream (Expr b)) -> ref -> SigTree ref

    TZip    :: (Typeable a, Typeable b) => Proxy (a, b) -> ref -> ref -> SigTree ref
    TFst    :: (Typeable a, Typeable b) => Proxy (a, b) -> ref        -> SigTree ref

    TDelay  :: (Typeable a) => Expr a   -> ref -> SigTree ref
    TSample ::                 Expr Int -> ref -> SigTree ref

class NewVar a
  where
    mkVar :: Dynamic -> a

--------------------------------------------------------------------------------
-- **

instance NewVar (Signal a)
  where
    mkVar = Var

instance MuRef (Signal (Expr a))
  where
    type DeRef (Signal (Expr a)) = SigTree

    mapDeRef f node = case node of
      (Const sf)   -> pure $ TConst sf
      (Lift  sf s) -> TLift sf <$> f s

      (Zip (s :: Signal (Expr x))
           (u :: Signal (Expr y)))      -> TZip (Proxy::Proxy(x, y)) <$> f s <*> f u
      (Fst (s :: Signal (Expr (x, y)))) -> TFst (Proxy::Proxy(x, y)) <$> f s

      (Delay  v s) -> TDelay  v <$> f s
      (Sample n s) -> TSample n <$> f s

      (Var _)      -> pure $ TVar

instance forall a b. (Typeable a, Typeable b)
    => MuRef (Signal (Expr a) -> Signal (Expr b))
  where
    type DeRef (Signal (Expr a) -> Signal (Expr b)) = SigTree

    mapDeRef f sf = let (v, sg) = capture sf
                     in TLambda (Proxy::Proxy (a, b))
                          <$> f v
                          <*> f sg

capture :: (Typeable a, Typeable b, NewVar a) => (a -> b) -> (a, b)
capture f = let a = mkVar (toDyn f) in (a, f a)
