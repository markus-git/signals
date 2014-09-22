{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE InstanceSigs #-}

module Temp where

import Prelude hiding (zip, fst, snd)
import qualified Prelude as P

--------------------------------------------------------------------------------

data Leaf a

data Expr a

data Stream a

data Signal a
  where
    Const :: Stream a -> Signal (Leaf a)
    Lift  :: (Stream a -> Stream b) -> Signal (Leaf a) -> Signal (Leaf a)
    Zip   :: Signal a -> Signal b -> Signal (a, b)
    Map   :: (Struct Expr a -> Struct Expr b) -> Signal a -> Signal b
    Fst   :: Signal (a, b) -> Signal a

type Sig a = Signal (Leaf a)

----------------------------------------

zip :: Signal a -> Signal b -> Signal (a, b)
zip = Zip

fst :: Signal (a, b) -> Signal a
fst = Fst

snd :: Signal (a, b) -> Signal b
snd = undefined -- fst . map swap

mapS :: (Struct Expr a -> Struct Expr b) -> Signal a -> Signal b
mapS = Map

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

instance (StructS a, StructS b) => StructS (a, b)
  where
    type Internal (a, b) = (Internal a, Internal b)

    fromS (a, b) = zip (fromS a) (fromS b)
    toS p = (toS (fst p), toS (snd p))

--------------------------------------------------------------------------------

data Struct c a
  where
    Leaf :: c a -> Struct c a
    Pair :: Struct c a -> Struct c b -> Struct c (a, b)

class StructE a
  where
    type InternalE a -- ~Internal

    fromE :: a -> Struct Expr (InternalE a)
    toE   :: Struct Expr (InternalE a) -> a

instance StructE (Expr a)
  where
    type InternalE (Expr a) = a

    fromE e = Leaf e
    toE (Leaf e) = e

instance (StructE a, StructE b) => StructE (a, b)
  where
    type InternalE (a, b) = (InternalE a, InternalE b)

    fromE (a, b) = Pair (fromE a) (fromE b)
    toE (Pair a b) = (toE a, toE b)

--------------------------------------------------------------------------------

type family FF s
type instance FF (Signal (e a)) = e a
type instance FF (a, b)         = (FF a, FF b)

lift
  :: ( StructS s1
     , StructS s2
     , StructE (FF s1)
     , StructE (FF s2)
     , Internal s1 ~ InternalE (FF s1) -- Fel!
     , Internal s2 ~ InternalE (FF s2) -- Fel!
     )
  => (FF s1 -> FF s2) -> s1 -> s2
lift f = toS . mapS (fromE . f . toE) . fromS

--------------------------------------------------------------------------------

-- addE :: Expr Int -> Expr Int -> Expr Int
-- addE = undefined

-- addS :: Signal (Expr Int) -> Signal (Expr Int) -> Signal (Expr Int)
-- addS = curry $ lift (\(a, b) -> addE a b)
