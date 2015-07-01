{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend.VHDL.Generate where

import Backend.VHDL.Syntax

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

--------------------------------------------------------------------------------
-- * Declaring Entities
--------------------------------------------------------------------------------

data EntityState = EntityState {
    entity_ports    :: PortList
  , entity_generics :: GenericList
  }

newtype Entity a = Entity { runEntityDeclaration :: StateT EntityState (Except String) a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState EntityState
           )

--------------------------------------------------------------------------------

declareEntity :: Identifier -> Entity a -> Declaration
declareEntity = undefined

--------------------------------------------------------------------------------

addPort :: Identifer -> Mode -> SubtypeIndication -> Entity a
addPort = undefined

addGeneric :: a -> Entity a
addGeneric = undefined

--------------------------------------------------------------------------------
-- * Declaring Types
--------------------------------------------------------------------------------

declareType :: Declaration
declareType = undefined

--------------------------------------------------------------------------------

-- ...

--------------------------------------------------------------------------------
