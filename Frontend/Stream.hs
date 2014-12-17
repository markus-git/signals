{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds    #-}

module Frontend.Stream where

import Core (CMD, newRef, getRef, setRef)
import Interpretation (VarPred)

import Control.Applicative
import Control.Monad
import Control.Monad.Operational

import Data.Typeable (Typeable)

import Prelude (($))
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Streams
--------------------------------------------------------------------------------

-- | ...
data Stream exp a
  where
    Stream :: Program (CMD exp) (Program (CMD exp) a) -> Stream exp a

-- | ...
type Str exp a = Stream exp (exp a)

--------------------------------------------------------------------------------
-- ** Instances

deriving instance Typeable Stream

--------------------------------------------------------------------------------
-- * User Interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Combinatorial functions

-- | creates and infinite stream by repeating @a@
repeat :: exp a -> Str exp a
repeat a = Stream $ return $ return a

-- | point-wise transform each value produced with @f@
map :: (exp a -> exp b) -> Str exp a -> Str exp b
map f (Stream init) = Stream $ fmap (fmap f) init

-- | joined two streams using @f@ to merge produced elements
zipWith :: (exp a -> exp b -> exp c)
        -> Str exp a -> Str exp b -> Str exp c
zipWith f (Stream init1) (Stream init2) = Stream $ do
  next1 <- init1
  next2 <- init2
  return $ do
    a <- next1
    b <- next2
    return $ f a b

--------------------------------------------------------------------------------
-- ** Sequential functions

-- | preappend @a@ to input stream
delay :: (Typeable a, VarPred exp a)
      => exp a -> Str exp a -> Str exp a
delay a (Stream init) = Stream $ do
  next <- init
  r    <- newRef a
  return $ do
    o <- getRef r
    v <- next
    setRef r v
    return o

--------------------------------------------------------------------------------
-- ** Run Functions

run :: Stream exp a -> Program (CMD exp) a
run (Stream init) = join init
