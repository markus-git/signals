{-# LANGUAGE GADTs #-}

module Backend.Ex where

import Data.Typeable

--------------------------------------------------------------------------------
-- * Existential types
--------------------------------------------------------------------------------

data Ex c
  where
    Ex :: Typeable a => c a -> Ex c

--------------------------------------------------------------------------------
-- ** Instances

instance Show (Ex c) where show _ = "Ex"
