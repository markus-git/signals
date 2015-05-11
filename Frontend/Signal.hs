{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE UndecidableInstances #-} -- !

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
    -- ^ ...
    Const :: (Typeable a, VarPred (IExp instr) a)
          => Stream instr (IExp instr a)
          -> Signal instr (Identity   a)

    Map   :: ( Typeable a
             , Typeable b
             )
          => (Stream instr a -> Stream instr b)
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

-- | `Shorthand` for signals which produce values of type `exp a`
newtype Sig instr a = Sig { runSig :: Signal instr (Identity a) }

--------------------------------------------------------------------------------
-- **

lift0 :: (e ~ IExp i, VarPred e a, Typeable a) => e a -> Sig i a
lift0 = Sig . Const . S.repeat

lift1 :: forall i e a b. (e ~ IExp i) => (e a -> e b) -> Sig i a -> Sig i b
lift1 f a = undefined

--------------------------------------------------------------------------------
-- * Nested Signals
--------------------------------------------------------------------------------

data Wit a
  where
    WE :: (Typeable a)
       => Wit (Identity a)
       
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

type family Packed (i :: (* -> *) -> * -> *) a :: *
type instance Packed i (Identity a) = Signal i (Identity a)
type instance Packed i (a, b)       = (Packed i a, Packed i b)

pack :: forall i a. Witness a => Signal i a -> Packed i a
pack s = go (wit :: Wit a) s
  where
    go :: Wit a -> Signal i a -> Packed i a
    go (WE)     s = s
    go (WP l r) s = (pack (Left s), pack (Right s))
    
unpack :: forall i a. Witness a => Packed i a -> Signal i a
unpack s = go (wit :: Wit a) s
  where
    go :: Wit a -> Packed i a -> Signal i a
    go (WE)     s = s
    go (WP l r) s = Join (unpack (fst s)) (unpack (snd s))

--------------------------------------------------------------------------------
-- **

type family Wrapped a :: *
type instance Wrapped (Identity a) = a
type instance Wrapped (a, b)       = (Wrapped a, Wrapped b)

wrap :: forall a. Witness a => a -> Wrapped a
wrap w = go (wit :: Wit a) w
  where
    go :: Wit a -> a -> Wrapped a
    go (WE)     a = runIdentity a
    go (WP l r) a = (wrap (fst a), wrap (snd a))

unwrap :: forall a. Witness a => Wrapped a -> a
unwrap w = go (wit :: Wit a) w
  where
    go :: Wit a -> Wrapped a -> a 
    go (WE)     a = Identity a
    go (WP l r) a = (unwrap (fst a), unwrap (snd a))

--------------------------------------------------------------------------------
-- **

lift :: forall proxy i a b.
       ( Witness a, Typeable a
       , Witness b, Typeable b
       )
    => proxy i a b
    -> (Wrapped  a -> Wrapped  b)
    -> (Packed i a -> Packed i b)
lift _ f = pack . Map (stream f :: Stream i a -> Stream i b) . unpack

stream :: forall i a b.
    ( Witness a
    , Witness b
    )
    => (Wrapped  a -> Wrapped  b)
    -> (Stream i a -> Stream i b)
stream f (Stream s) = Stream $ fmap (fmap (unwrap . f . wrap)) s

--------------------------------------------------------------------------------

{-
zipWith :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
zipWith f = curry $ liftAPA $ uncurry f

liftAPA :: ((a, b) -> c) -> (Signal a, Signal b) -> Signal c
liftAPA = undefined
-}
--------------------------------------------------------------------------------
{-
type family Instr a :: (* -> *) -> * -> *
type instance Instr (Sig    instr a) = instr
type instance Instr (Signal instr a) = instr
type instance Instr (Empty  instr a) = instr
type instance Instr (a, b)           = Instr a

-- | Conversion between signals and tuples
class Syntax a
  where
    type Internal a :: *
    
    sugar   :: a -> Signal (Instr a) (Internal a)
    desugar :: Signal (Instr a) (Internal a) -> a

--------------------------------------------------------------------------------

instance Syntax (Sig instr a)
  where
    type Internal (Sig instr a) = Empty instr a

    sugar   = unSig
    desugar = Sig

instance Syntax (Signal instr (Empty instr a))
  where
    type Internal (Signal instr (Empty instr a)) = Empty instr a

    sugar   = id
    desugar = id

instance ( Syntax a
         , Syntax b

         , Instr a ~ Instr b
         , Instr a ~ Instr (Internal a)
         , Instr b ~ Instr (Internal b)
{-
         , Rep (Internal a)
         , Rep (Internal b)
-}           
         , Typeable (IExp (Instr a))
         , Typeable (IExp (Instr b))
         , Typeable (Internal a)
         , Typeable (Internal b)
         )
    => Syntax (a, b)
  where
    type Internal (a, b) = (Internal a, Internal b)

    sugar   (a, b) = Join (sugar a) (sugar b)
    desugar s      = (desugar (Left s), desugar (Right s))

--------------------------------------------------------------------------------
-- **

-- | ...
lift :: ( Syntax a
        , Syntax b

        , Instr a ~ Instr b
        , Instr (Internal a) ~ Instr a
        , Instr (Internal b) ~ Instr b

        , Typeable (Internal a)
        , Typeable (Internal b)
        )
     => (Internal a -> Internal b) -> a -> b
lift f = desugar . Map (S.lift1 f) . sugar
-}
--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------
{-
-- | ..
class Rep a
  where
    represent :: Nested (Instr a) a -> Suple (Instr a) a
-}
--------------------------------------------------------------------------------
{-
instance (VarPred (IExp instr) a, Typeable a) => Rep (Empty instr a)
  where
    represent _ = Seaf ""

instance (Instr a ~ Instr b, Rep a, Rep b) => Rep (a, b)
  where
    represent (_ :: Nested (Instr a) (a, b)) = Sranch (represent left) (represent right)
      where
        left  = undefined :: Nested (Instr a) a
        right = undefined :: Nested (Instr b) b
-}
--------------------------------------------------------------------------------
-- ** 
{-
-- | ...
rep :: forall c instr a. (Rep a, Instr a ~ instr) => c instr a -> Suple instr a
rep _ = represent (error "rep" :: Nested instr a)
-}
