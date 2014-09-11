{-# LANGUAGE GADTs          #-}

-- {-# LANGUAGE DataKinds      #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE TypeOperators  #-}

module Frontend.Signal where

import Expr

import Data.Dynamic

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
           => Stream a
           -> Signal a

    Lift   :: (Typeable a, Typeable b)
           => (Stream a -> Stream b)
           ->  Signal a -> Signal b

    Zip    :: (Typeable a, Typeable b) => Signal a -> Signal b -> Signal (a, b)
    Fst    :: (Typeable a, Typeable b) => Signal (a, b) -> Signal a

    Delay  :: (Typeable a) => Expr a   -> Signal a -> Signal a
    Sample :: (Typeable a) => Expr Int -> Signal a -> Signal a

    -- |
    Var    :: Dynamic -> Signal a

----------------------------------------
-- Constructors

const :: (Typeable a) => Stream a -> Signal a
const = Const

lift :: (Typeable a, Typeable b) => (Stream a -> Stream b) -> Signal a -> Signal b
lift = Lift

zip :: (Typeable a, Typeable b) => Signal a -> Signal b -> Signal (a, b)
zip = Zip

fst :: (Typeable a, Typeable b) => Signal (a, b) -> Signal a
fst = Fst

delay :: (Typeable a) => Expr a -> Signal a -> Signal a
delay = Delay

sample :: (Typeable a) => Expr Int -> Signal a -> Signal a
sample = Sample

--------------------------------------------------------------------------------
-- **

repeat :: (Typeable a) => Expr a -> Signal a
repeat a = const $ S.repeat a

map :: (Typeable a, Typeable b) => (Expr a -> Expr b) -> Signal a -> Signal b
map f = lift (S.map f)

zipWith :: (Typeable a, Typeable b, Typeable c)
        => (Expr a -> Expr b -> Expr c)
        -> Signal a -> Signal b -> Signal c
zipWith f s1 s2 = map (unc f) $ zip s1 s2
  where unc f p = f (fstE p) (sndE p)

zipWith3 :: (Typeable a, Typeable b, Typeable c, Typeable d)
         => (Expr a -> Expr b -> Expr c -> Expr d)
         -> Signal a -> Signal b -> Signal c -> Signal d
zipWith3 f s1 s2 s3 = map (unc f) $ zip (zip s1 s2) s3
  where unc f p = f (fstE (fstE p)) (sndE (fstE p)) (sndE p)
