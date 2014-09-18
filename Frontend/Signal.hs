{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}

-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE DataKinds      #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE TypeOperators  #-}

module Frontend.Signal where

import Expr

import Data.Dynamic
import Data.Typeable (Typeable1)

import Frontend.Stream (Stream)
import qualified Frontend.Stream as S

import Prelude hiding (const, zip, fst, repeat, map, zipWith)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Signals
--------------------------------------------------------------------------------

-- todo: abstract over expr

data Signal a
  where
    Const  :: (Typeable a)
           => Stream (Expr a)
           -> Signal (Expr a)

    Lift   :: (Typeable a, Typeable b)
           => (Stream (Expr a) -> Stream (Expr b))
           ->  Signal (Expr a) -> Signal (Expr b)

    Zip    :: (Typeable a, Typeable b)
           => Signal (Expr a) -> Signal (Expr b) -> Signal (Expr (a, b))

    Fst    :: (Typeable a, Typeable b)
           => Signal (Expr (a, b)) -> Signal (Expr a)

    Delay  :: (Typeable a) => Expr a   -> Signal (Expr a) -> Signal (Expr a)
    Sample :: (Typeable a) => Expr Int -> Signal (Expr a) -> Signal (Expr a)

    -- |
    Var    :: Dynamic -> Signal a

--------------------------------------------------------------------------------
-- ** Constructors

const :: (Typeable a) => Stream (Expr a) -> Signal (Expr a)
const = Const

lift
  :: (Typeable a, Typeable b)
  => (Stream (Expr a) -> Stream (Expr b))
  -> Signal (Expr a) -> Signal (Expr b)
lift = Lift

zip
  :: (Typeable a, Typeable b)
  => Signal (Expr a) -> Signal (Expr b) -> Signal (Expr (a, b))
zip = Zip

fst
  :: (Typeable a, Typeable b)
  => Signal (Expr (a, b)) -> Signal (Expr a)
fst = Fst

delay :: (Typeable a) => Expr a -> Signal (Expr a) -> Signal (Expr a)
delay = Delay

sample :: (Typeable a) => Expr Int -> Signal (Expr a) -> Signal (Expr a)
sample = Sample

--------------------------------------------------------------------------------
-- **

deriving instance Typeable1 Signal

instance (Show a, Num a, Typeable a) => Num (Signal (Expr a))
  where
    fromInteger = repeat . fromInteger
    (+)         = zipWith (+)
    (*)         = zipWith (*)
    (-)         = zipWith (-)

--------------------------------------------------------------------------------
-- **

repeat :: (Typeable a) => Expr a -> Signal (Expr a)
repeat a = const $ S.repeat a

map :: (Typeable a, Typeable b)
    => (Expr a -> Expr b)
    -> Signal (Expr a)
    -> Signal (Expr b)
map f = lift (S.map f)

zipWith :: ( -- TupleExpr expr, Typeable1 expr
             Typeable a, Typeable b, Typeable c)
        => (Expr a -> Expr b -> Expr c)
        -> Signal (Expr a)
        -> Signal (Expr b)
        -> Signal (Expr c)
zipWith f s1 s2 = map (\p -> f (fstE p) (sndE p)) $ zip s1 s2
