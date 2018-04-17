{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Core.Witness where

import Control.Monad.Identity (Identity)

--------------------------------------------------------------------------------
-- * Check if a type is a tuple or not.
--------------------------------------------------------------------------------

-- | A representation of the 'tuple'-structure of a type.
data TupleRep pred a
  where
    -- ^ A base expression.
    Single :: pred a => TupleRep pred (Identity a)
    
    -- ^ A pair of two values, possibly tuples.
    Tuple  :: (Tuple pred a, Tuple pred b)
           => TupleRep pred a
           -> TupleRep pred b
           -> TupleRep pred (a, b)

--------------------------------------------------------------------------------

-- | Produce a witness of the 'tuple'-structure of a type.
class Tuple pred a
  where
    witness :: TupleRep pred a

-- | Witness for a base expression.
instance pred a => Tuple pred (Identity a)
  where
    witness = Single

-- | Witness for a tuple of values.
instance (Tuple pred a, Tuple pred b) => Tuple pred (a, b)
  where
    witness = Tuple (witness :: TupleRep pred a) (witness :: TupleRep pred b)

--------------------------------------------------------------------------------
