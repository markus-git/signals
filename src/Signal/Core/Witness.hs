{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Signal.Core.Witness where

import Control.Monad.Operational.Higher hiding (join)
import Control.Monad.Identity (Identity)
import Data.Typeable          (Typeable)
import Language.Embedded.VHDL (PredicateExp)

--------------------------------------------------------------------------------
-- * Witness
--------------------------------------------------------------------------------

-- | A witness for the correct construction (as a nested tuple) of some type
data Wit (i :: (* -> *) -> * -> *) a
  where
    WE :: (Typeable a, PredicateExp (IExp i) a) => Wit i (Identity a)
          
    WP :: (Witness i a, Witness i b)
       => Wit i a
       -> Wit i b
       -> Wit i (a, b)

--------------------------------------------------------------------------------
-- ** ...

-- | Class of things for which we can produce a correctness witness
class Typeable a => Witness i a
  where
    witness :: Typeable a => Wit i a

-- | Single value case
instance (Typeable a, PredicateExp (IExp i) a) => Witness i (Identity a)
  where
    witness = WE

-- | Nested tuple case
instance (Witness i a, Witness i b) => Witness i (a, b)
  where
    witness = WP (witness :: Wit i a) (witness :: Wit i b)

--------------------------------------------------------------------------------
