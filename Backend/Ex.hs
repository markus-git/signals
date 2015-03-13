{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Backend.Ex where

import Data.Typeable
import Data.Proxy

--------------------------------------------------------------------------------
-- * Existential types
--------------------------------------------------------------------------------

-- | Existential types over containers
data Ex c
  where
    Ex :: Typeable a => c a -> Ex c

-- | Wrapper type for nested containers
newtype (f :*: g) e = T (f (g e))

--------------------------------------------------------------------------------
-- ** Helper functinons for generalized existential types

-- | Hides the inner argument, wrapping the types
wrap :: Typeable e => f (g e) -> Ex (f :*: g)
wrap = Ex . T

-- | Retreives the inner type, uses type casting
unwrap :: Typeable e => Ex (f :*: g) -> f (g e)
unwrap (Ex t) = case gcast t of
                  Just (T x) -> x
                  Nothing    -> error "unwrap: type error"
