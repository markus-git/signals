{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.SignalObsv where

import           Frontend.Signal (Signal(..), Struct(..))
import qualified Frontend.Signal as Sig

import           Frontend.Stream (Stream)
import qualified Frontend.Stream as Str

import Control.Applicative hiding (Const)
import Data.Dynamic
import Data.Proxy
import Data.Typeable
import Data.Reify

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

data TSignal r
  where
    -- ^ Sig. func.
    TLambda :: r -> r -> TSignal r
    TVar    ::           TSignal r

    -- ^ Sig.
    TConst  ::  Stream a                   -> TSignal r
    TLift   :: (Stream a -> Stream b) -> r -> TSignal r

    TMap    :: T -> (Struct a -> Struct b) -> r -> TSignal r
    TZip    :: r -> r -> TSignal r
    TFst    :: r -> TSignal r
    TSnd    :: r -> TSignal r

    TDelay  :: a -> r -> TSignal r

    -- ^ Buffers
    TVBuff  ::               r -> TSignal r
    TDBuff  :: Num n => n -> r -> TSignal r
  deriving Typeable

data Tree e = L e | B (Tree e) (Tree e)

----------------------------------------

type T = Tree ()

-- |
structT :: forall a. Typeable a => Proxy a -> T
structT _ = go $ typeOf (undefined :: a)
  where
    ty   = typeRepTyCon $ typeOf (undefined :: (a, a))
    go r | typeRepTyCon r == ty = let [x,y] = typeRepArgs r in B (go x) (go y)
         | otherwise            = L ()

--------------------------------------------------------------------------------

instance MuRef (Signal a)
  where
    type DeRef (Signal a) = TSignal

    mapDeRef f node = case node of
      (Const sf)   -> pure $ TConst sf
      (Lift  sf s) -> TLift sf <$> f s

      (Map sf s)   -> TMap undefined sf <$> f s
      (Zip s  u)   -> TZip <$> f s <*> f u
      (Fst s)      -> TFst <$> f s
      (Snd s)      -> TSnd <$> f s

      (Delay a s)  -> TDelay a <$> f s

instance (Typeable a, Typeable b) => MuRef (Signal a -> Signal b)
  where
    type DeRef (Signal a -> Signal b) = TSignal

    mapDeRef f sf =
      let (v, sg) = let a = Var (toDyn sf) in (a, sf a)
       in TLambda <$> f v <*> f sg
