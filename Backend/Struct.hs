{-# LANGUAGE GADTs              #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Backend.Struct where

import Data.Typeable

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

data Empty a deriving Typeable

data Struct exp a
  where
    Leaf :: Typeable a => exp a -> Struct exp (Empty (exp a))
    Node :: Struct exp a
         -> Struct exp b
         -> Struct exp (a, b)
  deriving
    Typeable

--------------------------------------------------------------------------------
-- **

