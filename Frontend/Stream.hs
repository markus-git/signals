{-# LANGUAGE GADTs #-}

module Frontend.Stream where

import Core
import Expr

import Control.Applicative
import Control.Monad
import Control.Monad.Operational

import Prelude (($))
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Streams
--------------------------------------------------------------------------------

data Stream exp a
  where
    Stream :: Program (CMD exp) (Program (CMD exp) (exp a)) -> Stream exp a

--------------------------------------------------------------------------------
-- ** Interface

repeat :: exp a -> Stream exp a
repeat a = Stream $ return $ return a

map :: (exp a -> exp b) -> Stream exp a -> Stream exp b
map f (Stream init) = Stream $ fmap (fmap f) init

zipWith :: (exp a -> exp b -> exp c) -> Stream exp a -> Stream exp b -> Stream exp c
zipWith f (Stream init1) (Stream init2) = Stream $ do
  next1 <- init1
  next2 <- init2
  return $ do
    a <- next1
    b <- next2
    return $ f a b

--------------------------------------------------------------------------------
-- ** Run Functions

runStream :: Stream exp a -> Program (CMD exp) (exp a)
runStream (Stream init) = join init
