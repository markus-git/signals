{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Backend.Nested where

import Data.Functor.Identity
import Data.Typeable (Typeable) -- !

import Core -- !

--------------------------------------------------------------------------------
-- * Flexible Tuples
--------------------------------------------------------------------------------

-- | 0-value tuple
data Empty (instr :: (* -> *) -> * -> *) (a :: *)

-- | n-value tuple over instrontainers
data Tuple instr a
  where
    Leaf   :: (VarPred (IExp instr) a, Typeable a)
           => IExp instr a
           -> Tuple instr (Empty instr a)

    Branch :: Tuple instr a
           -> Tuple instr b
           -> Tuple instr (a, b)

--------------------------------------------------------------------------------
-- todo : merge with tuple

data Suple instr a
  where
    Seaf   :: (VarPred (IExp instr) a, Typeable a)
           => String
           -> Suple instr (Empty instr a)
           
    Sranch :: Suple instr a
           -> Suple instr b
           -> Suple instr (a, b)

data Ruple instr a
  where
    Reaf   :: (VarPred (IExp instr) a, Typeable a)
           => Ref a
           -> Ruple instr (Empty instr a)
           
    Rranch :: Ruple instr a
           -> Ruple instr b
           -> Ruple instr (a, b)

--------------------------------------------------------------------------------
