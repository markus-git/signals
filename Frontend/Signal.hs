{-# LANGUAGE GADTs          #-}

-- {-# LANGUAGE DataKinds      #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE TypeOperators  #-}

module Frontend.Signal where

import Frontend.Stream (Stream)
import qualified Frontend.Stream as S

import Prelude hiding (const, zip, fst, repeat, map, zipWith)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Signals
--------------------------------------------------------------------------------

data Signal exp a
  where
    Const :: Stream exp a -> Signal exp a
    Lift  :: (Stream exp a -> Stream exp b) -> Signal exp a -> Signal exp b

    Zip :: Signal exp a -> Signal exp b -> Signal exp (a, b)
    Fst :: Signal exp (a, b) -> Signal exp a

    Delay  :: exp a   -> Signal exp a -> Signal exp a
    Sample :: exp Int -> Signal exp a -> Signal exp a

const  = Const
lift   = Lift
zip    = Zip
fst    = Fst
delay  = Delay
sample = Sample

--------------------------------------------------------------------------------
-- **

repeat :: exp a -> Signal exp a
repeat a = const $ S.repeat a

map :: (exp a -> exp b) -> Signal exp a -> Signal exp b
map f = lift (S.map f)

--------------------------------------------------------------------------------
--

class TupExp exp
  where
    tup' :: exp a -> exp b -> exp (a, b)
    fst' :: exp (a, b) -> exp a
    snd' :: exp (a, b) -> exp b

zipWith :: TupExp exp
        => (exp a -> exp b -> exp c)
        -> Signal exp a -> Signal exp b -> Signal exp c
zipWith f s1 s2 = map (\p -> let a = fst' p
                                 b = snd' p
                              in f a b)
                $ zip s1 s2

zipWith3 :: TupExp exp
         => (exp a -> exp b -> exp c -> exp d)
         -> Signal exp a -> Signal exp b -> Signal exp c -> Signal exp d
zipWith3 f s1 s2 s3 = map (\p -> let a = fst' (fst' p)
                                     b = snd' (fst' p)
                                     c = snd' p
                                  in f a b c)
                    $ zip (zip s1 s2) s3
