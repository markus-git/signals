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

import           Frontend.Stream (Stream, Str)
import qualified Frontend.Stream as S

import Data.Dynamic (Dynamic)
import Prelude ( Eq, Show, String, ($), (.), id 
               , Num,        (+), (-), (*), fromInteger, abs, signum
               , Fractional, (/), fromRational, recip
               , Floating,   (**), pi, sin
               , curry, uncurry
               , undefined, error
               )

--------------------------------------------------------------------------------
-- * Signals
--------------------------------------------------------------------------------

data Signal (instr :: (* -> *) -> * -> *) (a :: *)
  where
    -- ^ ...
    Const :: (Typeable a, IPred instr a)
          => Stream instr (IExp instr a)
          -> Signal instr (Empty instr a)

    -- ^ ...
    Lift  :: (Typeable a, Typeable b, IPred instr a, IPred instr b)
          => (Stream instr (IExp instr a) -> Stream instr (IExp instr b))
          -> Signal instr (Empty instr a)
          -> Signal instr (Empty instr b)

    Delay :: (Typeable a, IPred instr a)
          => IExp   instr a
          -> Signal instr (Empty instr a)
          -> Signal instr (Empty instr a)

    -- ^ ...
    Map   :: ( Typeable a, Typeable b
             , Instr a ~ instr, Instr b ~ instr
             , Rep a, Rep b
             )
          => (Tuple instr a -> Tuple instr b)
          -> Signal instr a
          -> Signal instr b

    -- ^ ...
    Join  :: ( Typeable a, Typeable b
             , Instr a ~ instr, Instr b ~ instr
             , Rep a, Rep b
             )
          => Signal instr a
          -> Signal instr b
          -> Signal instr (a, b)

    -- ^ ...
    Left  :: ( Typeable a, Typeable b
             , Instr a ~ instr, Instr b ~ instr
             , Rep a, Rep b
             )
          => Signal instr (a, b)
          -> Signal instr a

    -- ^ ...
    Right :: ( Typeable a, Typeable b
             , Instr a ~ instr, Instr b ~ instr
             , Rep a, Rep b
             )
          => Signal instr (a, b)
          -> Signal instr b

    -- ^ ...
    Var   :: (Typeable a, Instr a ~ instr, Rep a)
          => Dynamic
          -> Signal instr a

-- | `Shorthand` for signals which produce values of type `exp a`
newtype Sig instr a = Sig { unSig :: Signal instr (Empty instr a) }

--------------------------------------------------------------------------------

repeat :: ( e ~ IExp instr, Typeable e
          , IPred instr a, Typeable a
          )
       => e a -> Sig instr a
repeat = Sig . Const . S.repeat

map :: ( e ~ IExp instr, Typeable e
       , IPred instr a, Typeable a
       , IPred instr b, Typeable b
       )
    => (e a -> e b) -> Sig instr a -> Sig instr b
map f = Sig . Lift (S.map f) . unSig

zipWith :: (e ~ IExp instr, Typeable e, Typeable instr
           , IPred instr a, Typeable a
           , IPred instr b, Typeable b
           , IPred instr c, Typeable c
           )
        => (e a -> e b -> e c) -> Sig instr a -> Sig instr b -> Sig instr c
zipWith f = curry $ lift $ uncurry f

--------------------------------------------------------------------------------

delay :: ( e ~ IExp instr, Typeable e
         , IPred instr a, Typeable a
         )
      => e a -> Sig instr a -> Sig instr a
delay a = Sig . Delay a . unSig

--------------------------------------------------------------------------------

instance ( Num (IExp instr a)
         , IPred instr a
         , Typeable (IExp instr)
         , Typeable instr
         , Typeable a
         ) =>
    Num (Sig instr a)
  where
    fromInteger = repeat . fromInteger
    (+)         = zipWith (+)
    (*)         = zipWith (*)
    (-)         = zipWith (-)

    abs = todo; signum = todo;

instance ( Fractional (IExp instr a)
         , IPred instr a
         , Typeable (IExp instr)
         , Typeable instr
         , Typeable a
         ) =>
    Fractional (Sig instr a)
  where
    fromRational = repeat . fromRational
    (/)          = zipWith (/)

    recip = todo;

todo = error "unsupported operation"

--------------------------------------------------------------------------------
-- * Nested Signals
--------------------------------------------------------------------------------

type family Instr  a :: (* -> *) -> * -> *
type instance Instr (Sig    instr a) = instr
type instance Instr (Signal instr a) = instr
type instance Instr (Empty  instr a) = instr
type instance Instr (a, b)           = Instr a

-- | Conversion between signals and tuples
class SSignal a
  where
    type Internal a :: *
    
    sugarS   :: a -> Signal (Instr a) (Internal a)
    desugarS :: Signal (Instr a) (Internal a) -> a

-- | Conversion betwee instructions and tuples
class STuple a
  where
    type Normal a :: *
    
    sugarE   :: Normal a -> Tuple (Instr a) a 
    desugarE :: Tuple (Instr a) a -> Normal a

--------------------------------------------------------------------------------

instance SSignal (Sig instr a)
  where
    type Internal (Sig instr a) = Empty instr a

    sugarS   = unSig
    desugarS = Sig

instance SSignal (Signal instr (Empty instr a))
  where
    type Internal (Signal instr (Empty instr a)) = Empty instr a

    sugarS   = id
    desugarS = id

instance ( SSignal a, SSignal b
         , Instr a ~ Instr b
         , Instr a ~ Instr (Internal a)
         , Instr b ~ Instr (Internal b)

         , Rep (Internal a)
         , Rep (Internal b)
           
         , Typeable (IExp (Instr a))
         , Typeable (IExp (Instr b))
         , Typeable (Internal a)
         , Typeable (Internal b)
         )
    => SSignal (a, b)
  where
    type Internal (a, b) = (Internal a, Internal b)

    sugarS   (a, b) = Join (sugarS a) (sugarS b)
    desugarS s      = (desugarS (Left s), desugarS (Right s))

--------------------------------------------------------------------------------

instance (IPred instr a, Typeable a) => STuple (Empty instr a)
  where
    type Normal (Empty instr a) = IExp instr a

    sugarE            = Leaf
    desugarE (Leaf a) = a

instance (STuple a, STuple b, Instr a ~ Instr b) => STuple (a, b)
  where
    type Normal (a, b) = (Normal a, Normal b)

    sugarE   (a, b)       = Branch (sugarE a) (sugarE b)
    desugarE (Branch a b) = (desugarE a, desugarE b)

--------------------------------------------------------------------------------
-- **

-- | ...
lift :: ( SSignal a
        , SSignal b
        , Instr a ~ Instr b
          
        , STuple (Internal a)
        , STuple (Internal b)
        , Instr  (Internal a) ~ Instr a
        , Instr  (Internal b) ~ Instr b

        , Rep (Internal a)
        , Rep (Internal b)
          
        , Typeable (Internal a)
        , Typeable (Internal b)
        , Typeable (IExp (Instr (Internal a)))
        , Typeable (IExp (Instr (Internal b)))
        )
     => (Normal (Internal a) -> Normal (Internal b)) -> a -> b
lift f = desugarS . Map (sugarE . f . desugarE) . sugarS

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | ..
class Rep a
  where
    represent :: Tuple (Instr a) a -> Suple (Instr a) a

--------------------------------------------------------------------------------

instance (IPred instr a, Typeable a) => Rep (Empty instr a)
  where
    represent _ = Seaf ""

instance (Instr a ~ Instr b, Rep a, Rep b) => Rep (a, b)
  where
    represent (_ :: Tuple (Instr a) (a, b)) = Sranch (represent left) (represent right)
      where
        left  = undefined :: Tuple (Instr a) a
        right = undefined :: Tuple (Instr b) b

--------------------------------------------------------------------------------
-- ** 

-- | ...
rep :: forall c instr a. (Rep a, Instr a ~ instr) => c instr a -> Suple instr a
rep _ = represent (undefined :: Tuple instr a)

