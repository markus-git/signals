{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}

module Temp where

import Expr

import Frontend.Stream (Stream)
import qualified Frontend.Stream as S

import Prelude (($),(.), undefined)
import qualified Prelude as P

--------------------------------------------------------------------------------

data Signal a

data Struct c a
  where
    Leaf :: c a -> Struct c a
    Pair :: Struct c a -> Struct c b -> Struct c (a, b)

data Struct' c expr a
  where
    Leaf' :: c (expr a) -> Struct' c expr a
    Pair' :: Struct' c expr a -> Struct' c expr b -> Struct' c expr (a, b)

mapS :: (Struct         expr a -> Struct         expr b)
     -> (Struct' Signal expr a -> Struct' Signal expr b)
mapS  = undefined

--------------------------------------------------------------------------------

class StructS a
  where
    type Internal a
    type Exp      a :: * -> *

    fromS :: a -> Struct' Signal (Exp a) (Internal a)
    toS   :: Struct' Signal (Exp a) (Internal a) -> a

instance StructS (Signal (e a))
  where
    type Internal (Signal (e a)) = a
    type Exp      (Signal (e a)) = e

    fromS s = Leaf' s
    toS (Leaf' s) = s

instance (StructS a, StructS b, Exp a ~ Exp b) => StructS (a, b)
  where
    type Internal (a, b) = (Internal a, Internal b)
    type Exp      (a, b) = Exp a -- ~ Exp b

    fromS (a, b) = Pair' (fromS a) (fromS b)
    toS (Pair' a b) = (toS a, toS b)

--------------------------------------------------------------------------------

class StructE a
  where
    type InternalE a
    type ExpE      a :: * -> *

    fromE :: a -> Struct (ExpE a) (InternalE a)
    toE   :: Struct (ExpE a) (InternalE a) -> a

instance StructE (Expr a)
  where
    type InternalE (Expr a) = a
    type ExpE      (Expr a) = Expr

    fromE e = Leaf e
    toE (Leaf e) = e

instance (StructE a, StructE b, ExpE a ~ ExpE b) => StructE (a, b)
  where
    type InternalE (a, b) = (InternalE a, InternalE b)
    type ExpE      (a, b) = ExpE a -- ~ ExpE b

    fromE (a, b) = Pair (fromE a) (fromE b)
    toE (Pair a b) = (toE a, toE b)

--------------------------------------------------------------------------------

type family FF s
type instance FF (Signal (e a)) = e a
type instance FF (a, b)         = (FF a, FF b)

lift
  :: ( StructS s1,      StructS s2
     , StructE (FF s1), StructE (FF s2)

     , Exp s1       ~ e, Exp s2       ~ e
     , ExpE (FF s1) ~ e, ExpE (FF s2) ~ e

     , Internal s1 ~ InternalE (FF s1)
     , Internal s2 ~ InternalE (FF s2)
     )
  => (FF s1 -> FF s2) -> s1 -> s2
lift f = toS . mapS (fromE . f . toE) . fromS
