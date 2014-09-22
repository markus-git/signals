{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE InstanceSigs #-}

module Frontend.Temp where

import Core
import Expr

import Frontend.Stream (Stream)
import qualified Frontend.Stream as S

import Control.Applicative
import Control.Monad
import Control.Monad.Operational
import Data.Typeable

import Prelude hiding (zip, fst, snd)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Signals
--------------------------------------------------------------------------------

data Leaf a

data Signal a
  where
    Const :: (Stream a)                                -> Signal (Leaf a)
    Lift  :: (Stream a -> Stream b) -> Signal (Leaf a) -> Signal (Leaf b)
    Map   :: (Struct a -> Struct b) -> Signal a        -> Signal b

    Zip   :: Signal a -> Signal b -> Signal (a, b)
    Fst   :: Signal (a, b) -> Signal a
    Snd   :: Signal (a, b) -> Signal b
  deriving Typeable

newtype Sig a = Sig { unSig :: Signal (Leaf a) }

liftS :: (Stream a -> Stream b) -> Signal (Leaf a) -> Signal (Leaf b)
liftS = Lift

mapS :: (Struct a -> Struct b) -> Signal a -> Signal b
mapS = Map

zipS :: Signal a -> Signal b -> Signal (a, b)
zipS = Zip

fstS :: Signal (a, b) -> Signal a
fstS = Fst

sndS :: Signal (a, b) -> Signal b
sndS = Snd

--------------------------------------------------------------------------------

class StructS a
  where
    type Internal a

    fromS :: a -> Signal (Internal a)
    toS   :: Signal (Internal a) -> a

instance StructS (Signal (Leaf a))
  where
    type Internal (Signal (Leaf a)) = Leaf a

    fromS = id
    toS   = id

instance StructS (Sig a)
  where
    type Internal (Sig a) = Leaf a

    fromS = unSig
    toS   = Sig

instance (StructS a, StructS b) => StructS (a, b)
  where
    type Internal (a, b) = (Internal a, Internal b)

    fromS (a, b) = zipS (fromS a) (fromS b)
    toS p        = (toS (fstS p), toS (sndS p))

--------------------------------------------------------------------------------

data Struct a
  where
    Leaf :: a -> Struct (Leaf a)
    Pair :: Struct a -> Struct b -> Struct (a, b)

class StructE a
  where
    type Normal a

    fromE :: Struct a -> Normal a
    toE   :: Normal a -> Struct a

instance StructE (Leaf a)
  where
    type Normal (Leaf a) = a
    fromE (Leaf a) = a
    toE a          = Leaf a

instance (StructE a, StructE b) => StructE (a, b)
  where
    type Normal (a, b) = (Normal a, Normal b)

    fromE (Pair a b) = (fromE a, fromE b)
    toE (a, b)       = Pair (toE a) (toE b)

--------------------------------------------------------------------------------

lift
  :: ( StructS s1
     , StructS s2
     , StructE (Internal s1)
     , StructE (Internal s2)
     )
  => (Normal (Internal s1) -> Normal (Internal s2)) -> s1 -> s2
lift f = toS . mapS (toE . f . fromE) . fromS

----------------------------------------

addE :: Expr Int -> Expr Int -> Expr Int
addE = undefined

addS :: Sig (Expr Int) -> Sig (Expr Int) -> Sig (Expr Int)
addS = curry $ lift $ uncurry addE

addS' :: Sig (Expr Int, Expr Int) -> Sig (Expr Int)
addS' = lift $ uncurry addE

addS'' :: (Sig (Expr Int), Sig (Expr Int)) -> Sig (Expr Int)
addS'' = lift $ uncurry addE

--------------------------------------------------------------------------------
-- * Observable
--------------------------------------------------------------------------------
