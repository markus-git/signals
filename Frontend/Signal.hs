{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Signal where

import Core
import Backend.Nested

import           Frontend.Stream (Stream(..), Str)
import qualified Frontend.Stream as S

import Data.Functor.Identity
import Data.Dynamic (Dynamic)
import Data.Proxy   (Proxy(..))
import Prelude ( Eq, Show, String, ($), (.), id 
               , Num,        (+), (-), (*), fromInteger, abs, signum
               , Fractional, (/), fromRational, recip
               , Floating,   (**), pi, sin
               , curry, uncurry
               , fmap
               , fst, snd
               , undefined, error
               )

--------------------------------------------------------------------------------
-- * Signals
--------------------------------------------------------------------------------

data Signal (instr :: (* -> *) -> * -> *) (a :: *)
  where
    -- ^ ... F e (Identity a) ~ (e a) ...
    Const :: ( Typeable a, VarPred e a
             , e ~ IExp instr
             )
          => Stream instr (e a)
          -> Signal instr (Identity a)

    -- ^ ...
    Map   :: ( Witness a, Typeable a --, VarPred e a
             , Witness b, Typeable b --, VarPred e b
             , e ~ IExp instr
             )
          => (   Stream instr (F instr a)
              -> Stream instr (F instr b))
          -> Signal instr a
          -> Signal instr b

    -- ^ ...
    Join  :: ( Witness a, Typeable a
             , Witness b, Typeable b
             )
          => Signal instr a
          -> Signal instr b
          -> Signal instr (a, b)

    -- ^ ...
    Left  :: ( Witness a, Typeable a
             , Witness b, Typeable b
             )
          => Signal instr (a, b)
          -> Signal instr a

    -- ^ ...
    Right :: ( Witness a, Typeable a
             , Witness b, Typeable b
             )
          => Signal instr (a, b)
          -> Signal instr b

    -- ^ ...
    Var   :: (Witness a, Typeable a) => Dynamic -> Signal instr a

-- | `Shorthand` for signals which produce values of type `exp a`
newtype Sig instr a = Sig { runSig :: Signal instr (Identity a) }

--------------------------------------------------------------------------------
-- **

lift0 :: (e ~ IExp i, VarPred e a, Typeable a) => e a -> Sig i a
lift0 = Sig . Const . S.repeat

lift1 :: forall i e a b.
       ( e ~ IExp i
       , Witness a, Typeable a --, VarPred e a
       , Witness b, Typeable b --, VarPred e b
       )
    => (e a -> e b) -> Sig i a -> Sig i b
lift1 f = lift z f
  where z = undefined :: proxy i (Identity a) (Identity b)

lift2 :: forall i e a b c.
       ( e ~ IExp i
       , Witness a, Typeable a --, VarPred e a
       , Witness b, Typeable b --, VarPred e b
       , Witness c, Typeable c --, VarPred e c
       )
    => (e a -> e b -> e c) -> Sig i a -> Sig i b -> Sig i c
lift2 f = curry $ lift z $ uncurry f
  where z = undefined :: proxy i (Identity a, Identity b) (Identity c)

--------------------------------------------------------------------------------
-- * Nested Signals
--------------------------------------------------------------------------------

data Wit a
  where
    WE :: Typeable a => Wit (Identity a)
          
    WP :: ( Witness a, Typeable a
          , Witness b, Typeable b
          )
          => Wit a -> Wit b -> Wit (a, b)

class Witness a
  where
    wit :: Wit a

instance Typeable a => Witness (Identity a)
  where
    wit = WE

instance forall a b.
    ( Witness a, Typeable a
    , Witness b, Typeable b
    )
    => Witness (a, b)
  where
    wit = WP (wit :: Wit a) (wit :: Wit b)

--------------------------------------------------------------------------------
-- **

type family F (i :: (* -> *) -> * -> *) (a :: *) :: *
type instance F i (Identity a) = IExp i a
type instance F i (a, b) = (F i a, F i b)

type family Packed (i :: (* -> *) -> * -> *) (a :: *) :: *
type instance Packed i (Identity a) = Sig i a
type instance Packed i (a, b)       = (Packed i a, Packed i b)

pack :: forall i a. Witness a => Signal i a -> Packed i a
pack s = go (wit :: Wit a) s
  where
    go :: Wit a -> Signal i a -> Packed i a
    go (WE)     s = Sig s
    go (WP l r) s = (,) (pack (Left s)) (pack (Right s))
    
unpack :: forall i a. Witness a => Packed i a -> Signal i a
unpack s = go (wit :: Wit a) s
  where
    go :: Wit a -> Packed i a -> Signal i a
    go (WE)     s = runSig s
    go (WP l r) s = Join (unpack (fst s)) (unpack (snd s))

--------------------------------------------------------------------------------
-- **

lift :: forall proxy i e a b.
        ( Witness a, Typeable a
        , Witness b, Typeable b
        , e ~ IExp i
        )
     => proxy i a b
     -> (F i a      -> F i b)
     -> (Packed i a -> Packed i b)
lift _ f = pack . h . unpack
  where
    g = stream f       :: Stream i (F i a) -> Stream i (F i b)
    h = Map (stream f) :: Signal i a       -> Signal i b

stream :: (a -> b) -> (Stream i a -> Stream i b)
stream f (Stream s) = Stream $ fmap (fmap f) s

--------------------------------------------------------------------------------
