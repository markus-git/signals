{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Signal.Core.Data
  ( Hide (..)
  , (:*:) (..)
  )
  where

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | A single constructor with a hidden type parameter.
data Hide f where
  Hide :: f a -> Hide f

-- | A pair of constructors applied to the same type parameter.
data (:*:) f g a where
  (:*:) :: f a -> g a -> (:*:) f g a

--------------------------------------------------------------------------------
