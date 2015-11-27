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
import Signal.Core.Reify      (Key(..))
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
import Language.Embedded.VHDL                 (Mode)
import Language.Embedded.VHDL.Expression.Type (Kind)
import qualified Language.VHDL                          as VHDL
import qualified Language.Embedded.VHDL                 as HDL
import qualified Language.Embedded.VHDL.Expression.Type as HDL

import System.Mem.StableName (eqStableName)

import Prelude hiding (Left, Right)

--------------------------------------------------------------------------------
-- * Compiler constructs
--------------------------------------------------------------------------------

data Scope   = Local | Global | Port

data Channel = Channel Identifier Kind

newtype Channels = Channels { runChannels :: IMap.IntMap Channel }

lookup :: Name a -> Channels -> Maybe Channel
lookup name (Channels m) = IMap.lookup (hash name) m

insert :: Name a -> Identifier -> Kind -> Channels -> Channels
insert name i k (Channels m) = Channels (IMap.insert (hash name) (Channel i k) m)

--------------------------------------------------------------------------------
-- ** ...

fromLinks
  :: forall i a.
     ( HDL.SequentialCMD (IExp i) :<: i
     , HDL.ConcurrentCMD (IExp i) :<: i
     , HDL.HeaderCMD     (IExp i) :<: i
     )
  => Key i (Identity a)
  -> Links i
  -> Program i Channels
fromLinks key = fromList key . filter useful . RMap.elems

--------------------------------------------------------------------------------

type M i = StateT Channels (Program i)

fromList
  :: forall i a.
     ( HDL.SequentialCMD (IExp i) :<: i
     , HDL.ConcurrentCMD (IExp i) :<: i
     , HDL.HeaderCMD     (IExp i) :<: i
     )
  => Key i (Identity a)
  -> [Entry (Linked i)]
  -> Program i Channels
fromList (Key key) es =
    CMS.execStateT (mapM_ go es) (Channels IMap.empty)
  where
    go :: Entry (Linked i)  -> M i ()
    go (RMap.Entry name (Linked node o@(Link n))) =
      case node of
        Var d -> do
          init o
        Repeat c -> do
          declare n HDL.Variable Local (Just c)
        Map f s -> do
          init o
        Delay d s -> do
          declare n         HDL.Signal Global (Just d)
          declare (other n) HDL.Signal Global (Nothing)
          -- ??? 'opposite' ???
          -- declare ? (Nothing)
        Mux s cs -> do
          init o

    init :: forall b. Link i b -> M i ()
    init (Link names) = dist (witness :: Wit i b) names
      where
        dist :: Wit i x -> Names (S Symbol i x) -> M i ()
        dist (WE)     (name) =
          do declare name HDL.Variable Local Nothing
        dist (WP l r) (u, v) =
          do dist l u
             dist r v

    declare
      :: forall b.
         ( HDL.SequentialCMD (IExp i) :<: i
         , HDL.ConcurrentCMD (IExp i) :<: i
         , HDL.PredicateExp  (IExp i) b)
      => Named (S Symbol i (Identity b))
      -> Kind
      -> Scope
      -> Maybe (IExp i b)
      -> M i ()
    declare (Named n) kind scope e
      | key `eqStableName` n =
          do i <- CMS.lift $ HDL.signalPort HDL.In e
             CMS.modify (insert n i HDL.Signal)
      | otherwise =
          do i <- CMS.lift $ case kind of
               HDL.Variable -> case scope of
                 Local  -> HDL.variableL e
               HDL.Signal -> case scope of
                 Port   -> HDL.signalPort HDL.Out e
                 Global -> HDL.signalG e
             CMS.modify (insert n i undefined)  

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
