{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Frontend.Stream where

import Core
import Expr

import Control.Applicative
import Control.Monad
import Control.Monad.Operational
import Data.Typeable (Typeable1)

import Prelude (($))
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Streams
--------------------------------------------------------------------------------

-- todo: abstract over expr

data Stream a
  where
    Stream :: Program (CMD Expr) (Program (CMD Expr) a) -> Stream a

deriving instance Typeable1 Stream

--------------------------------------------------------------------------------
-- ** Interface

repeat :: expr a -> Stream (expr a)
repeat a = Stream $ return $ return a

map :: (expr a -> expr b) -> Stream (expr a) -> Stream (expr b)
map f (Stream init) = Stream $ fmap (fmap f) init

zipWith :: (expr a -> expr b -> expr c)
        -> Stream (expr a)
        -> Stream (expr b)
        -> Stream (expr c)
zipWith f (Stream init1) (Stream init2) = Stream $ do
  next1 <- init1
  next2 <- init2
  return $ do
    a <- next1
    b <- next2
    return $ f a b

--------------------------------------------------------------------------------
-- ** Run Functions

runStream :: Stream a -> Program (CMD Expr) a
runStream (Stream init) = join init
