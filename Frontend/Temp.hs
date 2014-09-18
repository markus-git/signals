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

class SStruct a
  where
    type Internal a
    type Exp      a :: * -> *

    fromS :: a -> Struct' Signal (Exp a) (Internal a)
    toS   :: Struct' Signal (Exp a) (Internal a) -> a
