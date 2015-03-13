{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ConstraintKinds #-}

module Core (
    module Core
  , module Control.Monad.Operational.Compositional
  , module Language.Embedded.Imperative
  , module Data.Typeable
  )
  where

import Control.Monad.Operational.Compositional
import Language.Embedded.Imperative
import Data.Typeable

--------------------------------------------------------------------------------
-- * Core
--------------------------------------------------------------------------------

-- CMD is hardcodede for now

-- | Predicate used
type P exp = (Typeable :/\: VarPred exp)

-- | Instruction sets used
type CMD exp = RefCMD (P exp) exp :+: ControlCMD exp

-- | Shorthand
type Prog exp = Program (Tag (P exp) exp (CMD exp))
