{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Stream where

import Core
import Control.Applicative
import Control.Monad
import Prelude (($))

--------------------------------------------------------------------------------
-- * Streams
--------------------------------------------------------------------------------

-- | ...
data Stream exp a where Stream :: Prog exp (Prog exp a) -> Stream exp a

-- | ...
type Str exp a = Stream exp (exp a)

--------------------------------------------------------------------------------
-- ** Constructors

-- | creates a stream from a program
stream :: Prog exp (Prog exp (exp a)) -> Str exp a
stream = Stream

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
delay :: P exp a => exp a -> Str exp a -> Str exp a
delay a (Stream init) = Stream $ do
  next <- init
  r    <- initRef a
  return $ do
    o <- getRef r
    v <- next
    setRef r v
    return o

--------------------------------------------------------------------------------
-- ** Run Functions

-- | ...
run :: Stream exp a -> Prog exp a
run (Stream init) = join init
