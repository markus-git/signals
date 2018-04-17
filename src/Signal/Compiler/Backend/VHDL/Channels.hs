{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler.Backend.VHDL.Channels where
{-
  ( Channels
  , Channel(..)

  , lookup
  , insert
  , with

  , declareSignals
  , declareVariables
  )
-}

import Signal.Core
--import Signal.Core.Witness
import Signal.Core.Reify
import Signal.Compiler.Linker
import Signal.Compiler.Linker.Names

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

-- operational-alacarte
import Control.Monad.Operational.Higher (Program)
import Data.ALaCarte (Param2(..))

-- hardware-edsl
import qualified Language.Embedded.Hardware.Interface as HDL

{-
import Language.VHDL                     (Identifier)
import Language.Embedded.VHDL            (Mode)
import Language.Embedded.VHDL.Monad.Type (Kind)
import qualified Language.VHDL                     as VHDL
import qualified Language.Embedded.VHDL            as HDL
import qualified Language.Embedded.VHDL.Monad.Type as HDL
-}
import System.Mem.StableName (eqStableName)

import Prelude hiding (Left, Right, lookup)

--------------------------------------------------------------------------------
-- * Compiler constructs
--------------------------------------------------------------------------------

-- | Channels consists of either signals or variables.
data Kind = Signal | Variable

-- | A Channel is represented by unique identifier to a signal or variable.
data Channel = Channel HDL.VarId Kind

-- | Short-hand for a int-mapping over channels.
newtype Channels = Channels { runChannels :: IMap.IntMap Channel }

-- | Short-hand for a state monad over channels.
type ChannelsM instr exp pred = StateT Channels (Program instr (Param2 exp pred))

--------------------------------------------------------------------------------

-- | Lookup a channel.
lookup :: RMap.Name a -> Channels -> Maybe Channel
lookup name (Channels m) = IMap.lookup (hash name) m

-- | Insert new channel of given kind
insert :: RMap.Name a -> HDL.VarId -> Kind -> Channels -> Channels
insert name i k (Channels m) = Channels (IMap.insert (hash name) (Channel i k) m)

-- | Left join of two channel sets
union :: Channels -> Channels -> Channels
union (Channels m) (Channels n) = Channels (IMap.unionWith const m n)

--------------------------------------------------------------------------------

-- | Declare all signal inputs/outputs of a given link-mapping.
declareSignals :: Witness pred
  => Key exp pred a
  -> Links exp pred
  -> Program instr (Param2 exp pred) Channels
declareSignals (Key k) links =
    CMS.execStateT (mapM_ declareSignal (RMap.elems links)) (Channels IMap.empty)
  where
    declareSignal :: RMap.Entry (LinkedNode exp pred) -> ChannelsM instr exp pred ()
    declareSignal (RMap.Entry name (LinkedNode node))
      | name `eqStableName` k = declareRoot   node
      | otherwise             = declareGlobal node
    
    declareRoot :: Core Link exp pred a -> ChannelsM instr exp pred ()
    declareRoot (Val e) = undefined

    declareGlobal :: Core Link exp pred a -> ChannelsM instr exp pred ()
    declareGlobal = undefined
{-
-- | Declare a signal.
--
-- A number of special cases are included in order to account for root nodes.
--   > Roots are always uninitialized output ports, unless
--     * It's a Repeat node, then it is initialized
--     * It's a Delay  node, then a initialized signal is also declared
--     * It's a Var    node, then the node is both an input and output signal
--   > If they aren't roots, then
--     * Variable nodes are still declared as ports
--     * Delays produce two signals
declareSignals
  :: forall i a.
     ( HDL.HeaderCMD     (IExp i) :<: i
     , HDL.ConcurrentCMD (IExp i) :<: i
     )
  => Key i (Identity a)
  -> Links i
  -> Program i Channels
declareSignals (Key key) links =
    CMS.execStateT (mapM_ declare (RMap.elems links)) (Channels IMap.empty)
  where
    declare :: Entry (Linked i) -> M i ()
    declare (RMap.Entry name (Linked node o@(Link single)))
      | name `eqStableName` key = case node of
          Repeat  c -> do
            port   (single) HDL.Out (Just c)
          Delay d s -> do
            port   (other single) HDL.Out (Nothing)
            signal (single) (Just d)
          Var   d   -> nested HDL.InOut o
          _         -> nested HDL.Out   o
      | otherwise = case node of
          Var   d   -> nested HDL.In o
          Delay d s -> do
            signal (other single) (Nothing)
            signal (single)       (Just d)
          _         -> return ()

    nested :: forall b. Mode -> Link i b -> M i ()
    nested mode (Link names) = dist (witness :: Wit i b) names
      where
        dist :: Wit i x -> Names (S Symbol i x) -> M i ()
        dist (WP l r) (u, v) = dist l u >> dist r v
        dist (WE)     (name) = port name mode (Nothing :: Maybe (IExp i x))
-}
--------------------------------------------------------------------------------
-- ** ...
{-
-- | ...
declareVariables :: forall i a . (HDL.SequentialCMD (IExp i) :<: i)
  => Key i (Identity a)
  -> Links i
  -> Program i Channels
declareVariables (Key key) links =
    CMS.execStateT (mapM_ declare (RMap.elems links)) (Channels IMap.empty)
  where
    declare :: Entry (Linked i) -> M i ()
    declare (RMap.Entry name (Linked node o@(Link single)))
      | name `eqStableName` key = return () -- it's already declared
      | otherwise = case node of
          Repeat c -> variable single (Just c)
          Map _ _  -> nested o
          Mux _ _  -> nested o
          _        -> return ()

    nested :: forall b. Link i b -> M i ()
    nested (Link names) = dist (witness :: Wit i b) names
      where
        dist :: Wit i x -> Names (S Symbol i x) -> M i ()
        dist (WP l r) (u, v) = dist l u >> dist r v
        dist (WE)     (name) = variable name (Nothing :: Maybe (IExp i x))
-}
--------------------------------------------------------------------------------
{-
-- | ...
port :: forall i a . (HDL.HeaderCMD (IExp i) :<: i, HDL.PredicateExp (IExp i) a)
  => Named (S Symbol i (Identity a))
  -> Mode
  -> Maybe (IExp i a)
  -> M i ()
port name mode exp = do
  i <- CMS.lift (HDL.signalPort mode exp)
  CMS.modify (insert name i HDL.Signal)

-- | ...
signal
  :: forall i a.
     ( HDL.ConcurrentCMD (IExp i) :<: i
     , HDL.PredicateExp  (IExp i) a
     )
  => Named (S Symbol i (Identity a))
  -> Maybe (IExp i a)
  -> M i ()
signal name exp = do
  i <- CMS.lift (HDL.signalG exp)
  CMS.modify (insert name i HDL.Signal)

-- | ...
variable
  :: forall i a.
     ( HDL.SequentialCMD (IExp i) :<: i
     , HDL.PredicateExp  (IExp i) a
     )
  => Named (S Symbol i (Identity a))
  -> Maybe (IExp i a)
  -> M i ()
variable name exp = do
  i <- CMS.lift (HDL.variableL exp)
  CMS.modify (insert name i HDL.Variable)
-}
--------------------------------------------------------------------------------
