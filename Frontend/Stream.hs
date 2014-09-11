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

-- todo: abstract over expr

data Stream a
  where
    Stream :: Program (CMD Expr) (Program (CMD Expr) (Expr a)) -> Stream a

--------------------------------------------------------------------------------
-- ** Interface

repeat :: Expr a -> Stream a
repeat a = Stream $ return $ return a

map :: (Expr a -> Expr b) -> Stream a -> Stream b
map f (Stream init) = Stream $ fmap (fmap f) init

zipWith :: (Expr a -> Expr b -> Expr c) -> Stream a -> Stream b -> Stream c
zipWith f (Stream init1) (Stream init2) = Stream $ do
  next1 <- init1
  next2 <- init2
  return $ do
    a <- next1
    b <- next2
    return $ f a b

--------------------------------------------------------------------------------
-- ** Run Functions

runStream :: Stream a -> Program (CMD Expr) (Expr a)
runStream (Stream init) = join init
