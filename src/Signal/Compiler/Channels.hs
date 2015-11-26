{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler.Channels
{-  ( Identified(..)
  , Identifiers
  , Kind
  , Scope   (..)
  , Ident   (..)
  , Channel (..)
  , Channels(..)

  , fromLinks
  )-}
  where

import Signal.Core            (S(..), E(..), Symbol)
import Signal.Core.Witness
import Signal.Compiler.Linker

import Control.Monad.Operational.Higher
import Control.Monad
import Control.Monad.Identity (Identity)
import Control.Monad.State    (StateT)
import qualified Control.Monad.State as CMS

import Data.Maybe    (fromJust)
import Data.Typeable
import Data.Hashable
import Data.Ref
import Data.Ref.Map  (Map, Entry, Name)
import qualified Data.Ref.Map as RMap
import qualified Data.IntMap  as IMap

import Language.VHDL                          (Identifier)
import Language.Embedded.VHDL.Expression.Type (Kind)
import qualified Language.VHDL                          as VHDL
import qualified Language.Embedded.VHDL                 as HDL
import qualified Language.Embedded.VHDL.Expression.Type as HDL

import Prelude hiding (Left, Right)

--------------------------------------------------------------------------------
-- * Compiler constructs
--------------------------------------------------------------------------------

newtype Channels = Channels { runChannels :: IMap.IntMap Identifier }

lookup :: Name a -> Channels -> Maybe Identifier
lookup name (Channels m) = IMap.lookup (hash name) m

insert :: Name a -> Identifier -> Channels -> Channels
insert name ident (Channels m) = Channels (IMap.insert (hash name) ident m)

--------------------------------------------------------------------------------
-- ** ...

fromLinks
  :: forall i.
     ( HDL.SequentialCMD (IExp i) :<: i
     , HDL.ConcurrentCMD (IExp i) :<: i
     )
  => Links i
  -> Program i Channels
fromLinks = fromList . filter useful . RMap.elems

--------------------------------------------------------------------------------

type M i = StateT Channels (Program i)

fromList
  :: forall i.
     ( HDL.SequentialCMD (IExp i) :<: i
     , HDL.ConcurrentCMD (IExp i) :<: i
     )
  => [Entry (Linked i)]
  -> Program i Channels
fromList es = CMS.execStateT (mapM_ go es) (Channels IMap.empty)
  where
    go :: Entry (Linked i)  -> M i ()
    go (RMap.Entry name (Linked node o@(Link n))) =
      case node of
        Repeat c -> do
          declare n (Just c)
        Map f s -> do
          init o
        Delay d s -> do
          declare n (Just d)
          -- ??? 'opposite' ???
        Mux s cs -> do
          init o

    init :: forall a. Link i a -> M i ()
    init (Link names) = dist (witness :: Wit i a) names
      where
        dist :: Wit i x -> Names (S Symbol i x) -> M i ()
        dist (WE)     (name) = declare name Nothing
        dist (WP l r) (u, v) = dist l u >> dist r v

-- | Declares a VHDL variable for a name with, possibly, some init value
declare
  :: ( HDL.SequentialCMD (IExp i) :<: i
     , HDL.PredicateExp  (IExp i) a)
  => Named (S sym i (Identity a))
  -> Maybe (IExp i a)
  -> M i ()
declare (Named n) e = do
  i <- CMS.lift $ HDL.variableL e
  CMS.modify (insert n i)  

-- | Usefulness refers to whether we should generate code for the node or not
useful :: RMap.Entry (Linked i) -> Bool
useful (RMap.Entry name (Linked node link)) =
  case node of
    Var    {} -> True
    Repeat {} -> True
    Map    {} -> True
    Delay  {} -> True
    Mux    {} -> True
    _         -> False

--------------------------------------------------------------------------------
