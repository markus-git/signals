{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Backend.Nested where

import Data.Functor.Identity

--------------------------------------------------------------------------------
-- * Flexible Tuples
--------------------------------------------------------------------------------

-- | 0-value tuple
data Empty a

-- | n-value tuple over containers
data Tuple c a
  where
    Leaf   :: c a -> Tuple c (Empty (c a))
    Branch :: Tuple c a -> Tuple c b -> Tuple c (a, b)
