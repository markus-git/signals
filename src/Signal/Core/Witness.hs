{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Signal.Core.Witness where

import Control.Monad.Operational.Compositional hiding (join)
import Control.Monad.Identity (Identity)
import Data.Typeable          (Typeable)
import Language.Embedded.VHDL (PredicateExp)

--------------------------------------------------------------------------------
-- * Witness
--------------------------------------------------------------------------------

-- | ...
data Wit (i :: (* -> *) -> * -> *) a
  where
    WE :: (Typeable a, PredicateExp (IExp i) a)
       => Wit i (Identity a)
          
    WP :: (Witness i a, Witness i b)
       => Wit i a
       -> Wit i b
       -> Wit i (a, b)

--------------------------------------------------------------------------------
-- ** ...

-- | ...
class Typeable a => Witness i a
  where
    witness :: Typeable a => Wit i a

-- | ...
instance (Typeable a, PredicateExp (IExp i) a) => Witness i (Identity a)
  where
    witness = WE

-- | ...
instance (Witness i a, Witness i b) => Witness i (a, b)
  where
    witness = WP (witness :: Wit i a) (witness :: Wit i b)

--------------------------------------------------------------------------------
