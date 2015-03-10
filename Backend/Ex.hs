{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

---------------------------------------- Testing
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
----------------------------------------

module Backend.Ex where

import Data.Typeable
import Data.Proxy

---------------------------------------- Testing
import Frontend.Signal (TStruct(..))
----------------------------------------

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
-- ** Instances

instance Show (Ex c) where show _ = "Ex"

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

--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

instance Show (Ex (TStruct e))
  where
    show (Ex s) = showTS s

showTS :: TStruct e a -> String
showTS (TLeaf c) = show c
showTS (TPair l r) = "(" ++ showTS l ++ "," ++ showTS r ++ ")"
