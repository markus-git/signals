{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
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

import Signal.Core (Core, Symbol)
import Signal.Core.Witness
import Signal.Core.Reify
import Signal.Core.Data
import Signal.Compiler.Linker
import qualified Signal.Core as S
import qualified Signal.Compiler.Linker.Names as Names

import Control.Monad.Operational.Higher
import Control.Monad
import Control.Monad.Identity (Identity)
import Control.Monad.Reader   (Reader)
import qualified Control.Monad.Reader as CMR

import Type.Reflection
import Data.Maybe    (catMaybes)
import Data.Typeable hiding (TypeRep)
import Data.Constraint
import Data.Dynamic
import Data.Hashable
import Data.Ref
import Data.Ref.Map  (Map, Entry, Name)
import qualified Data.Ref.Map as RMap
import qualified Data.IntMap  as IMap

-- operational-alacarte
import Control.Monad.Operational.Higher (Program)
import Data.ALaCarte (Param2(..))

-- hardware-edsl
import qualified Language.Embedded.Hardware.Command   as HDL
import qualified Language.Embedded.Hardware.Interface as HDL

import System.Mem.StableName -- !
import Unsafe.Coerce -- !!!

import Prelude hiding (Left, Right, lookup)

--------------------------------------------------------------------------------
-- * Compiler constructs
--------------------------------------------------------------------------------

-- | A Channel is represented by either a signal or a variable.
data Channel pred a
  where
    S :: pred a => HDL.Signal   a -> Channel pred a
    V :: pred a => HDL.Variable a -> Channel pred a

-- | Like bundle for names, but for channels.
data Bundle pred a
  where
    -- ^ A single channel.
    Chan :: Channel pred a
         -> Bundle  pred (Core Symbol exp pred (Identity a))
    -- ^ A single, but delayed, channel.
    Buff :: Channel pred a -- rin
         -> Channel pred a -- r
         -> Bundle  pred (Core Symbol exp pred (Identity a))
    -- ^ A pair of channels.
    Pair :: Bundle  pred (Core Symbol exp pred a)
         -> Bundle  pred (Core Symbol exp pred b)
         -> Bundle  pred (Core Symbol exp pred (a, b))

--------------------------------------------------------------------------------

data Wire exp pred a
  where
    Wire :: Bundle pred (Core Symbol exp pred a) -> Wire exp pred a

data Wired exp pred a
  where
    Wired ::
         Maybe (Core Wire exp pred a) -- ^ Symbol with incoming channels.
      -> Maybe (Wire exp pred a)      -- ^ Outgoing channels for symbol.
      -> Core Link exp pred a         -- ^ Symbol to wire.
      -> Wired exp pred (Core Symbol exp pred a)

--------------------------------------------------------------------------------

-- | Join two wires.
joinWires :: Wire exp pred a -> Wire exp pred b -> Wire exp pred (a, b)
joinWires (Wire a) (Wire b) = Wire (Pair a b)

-- | Read the left wire.
leftWire :: Wire exp pred (a, b) -> Wire exp pred a
leftWire (Wire (Pair a b)) = Wire a

-- | Read the right wire.
rightWire :: Wire exp pred (a, b) -> Wire exp pred b
rightWire (Wire (Pair a b)) = Wire b

-- | From a buffered wire, read the 'r' wire.
bufferedWire :: Wire exp pred a -> Wire exp pred a
bufferedWire (Wire (Buff rin r)) = Wire (Chan r)

-- | Get the name of a bundle.
nameOf :: Names.Bundle (Core Symbol exp pred (Identity a)) -> String
nameOf (Names.One n) = Names.toString n

-- | Get the buffered name of bundle.
otherOf :: Names.Bundle (Core Symbol exp pred (Identity a)) -> String
otherOf (Names.One n) = Names.toString $ Names.other n

--------------------------------------------------------------------------------

type Input = Hide (HDL.Signal)

input :: HDL.Signal a -> Input
input = Hide

-- | Fetch the corresponding input signal for a dynamic object.
signalOf :: forall a . Proxy (Identity a) -> Dynamic -> [Input] -> HDL.Signal a
signalOf _ dyn is = go (indexOf dyn) is
  where
    go :: Int -> [Input] -> HDL.Signal a
    go 0 _  = error "signals.channels: access to input from constant signal."
    go i is = case is !! (i - 1) of
      Hide sig -> unsafeCoerce sig

-- | Check the arity of a dynamic object, that is, check what input the object
--   corresponds to.
indexOf :: Dynamic -> Int
indexOf (Dynamic trep _) = go trep
  where
    go :: TypeRep a -> Int
    go (Con _)       = 0
    go (App _ _)     = 0
    go (Fun arg res) = 1 + go arg

--------------------------------------------------------------------------------
-- ** Declaration of wires.
--------------------------------------------------------------------------------

-- | Declare any signal used by a wired node.
declareSignalNodes :: forall instr exp pred. (HDL.SignalCMD :<: instr)
  => RMap.Entry (LinkedNode exp pred)
  -> [Input]
  -> Program instr (Param2 exp pred) (Maybe (RMap.Entry (Wired exp pred)))
declareSignalNodes (RMap.Entry name (LinkedNode core (Link link))) is =
  let entry w = RMap.Entry name (Wired Nothing (Just (Wire w)) core) in
  case core of
    S.Var   d   -> return $ Just $ entry $ Chan $ S $ signalOf Proxy d is
    S.Val   _   -> return Nothing
    S.Map   _ _ -> return Nothing
    S.Delay e _ -> do
      let n = nameOf  link
      let o = otherOf link
      rin <- HDL.newNamedSignal n
      r   <- HDL.initNamedSignal o $ S.lit (dict e) e
      return $ Just $ entry $ Buff (S rin) (S r)
    _           -> return Nothing
  where
    dict :: pred a => a -> Dict (pred a)
    dict _ = Dict

-- | Declare all signals used during linking.
declareSignals :: (HDL.SignalCMD :<: instr)
  => Links exp pred
  -> [Input]
  -> Program instr (Param2 exp pred) (Map (Wired exp pred))
declareSignals links is =
  do signals <- mapM (flip declareSignalNodes is) $ RMap.toList links
     return $ RMap.fromList $ catMaybes signals

--------------------------------------------------------------------------------

-- | Declare any variable used by a wired node.
declareVariableNodes :: forall instr exp pred . (HDL.VariableCMD :<: instr)
  => RMap.Entry (LinkedNode exp pred)
  -> Program instr (Param2 exp pred) (Maybe (RMap.Entry (Wired exp pred)))
declareVariableNodes (RMap.Entry name (LinkedNode core (Link link))) =
  let entry w = RMap.Entry name (Wired Nothing (Just (Wire w)) core) in
  case core of
    S.Var   _   -> return Nothing
    S.Val   e   -> do
      var <- HDL.initNamedVariable (nameOf link) e
      return $ Just $ entry $ Chan $ V var
    S.Map   _ _ -> do
      wires <- declareVariableBundle witness link
      return $ Just $ entry wires
    S.Delay _ _ -> return Nothing
    _           -> return Nothing

-- | Declare all variables used in a bundle of variable links.
declareVariableBundle :: (HDL.VariableCMD :<: instr)
  => TupleRep pred a
  -> Names.Bundle (S.Core Symbol exp pred a)
  -> Program instr (Param2 exp pred) (Bundle pred (S.Core Symbol exp pred a))
declareVariableBundle (Single) (Names.One n) =
  do var <- HDL.variable $ Names.toString n
     return $ Chan $ V var
declareVariableBundle (Tuple l r) (Names.Pair n m) =
  do vl <- declareVariableBundle l n
     vr <- declareVariableBundle r m
     return $ Pair vl vr

-- | Declare all variables used during linking.
declareVariables :: (HDL.VariableCMD :<: instr)
  => Links exp pred
  -> Program instr (Param2 exp pred) (Map (Wired exp pred))
declareVariables links =
  do variables <- mapM declareVariableNodes $ RMap.toList links
     return $ RMap.fromList $ catMaybes variables

--------------------------------------------------------------------------------
-- ** Linking of wires.
--------------------------------------------------------------------------------

-- | Wiring monad.
type WireM exp pred = Reader (Map (Wired exp pred))

-- | Lookup a wire.
lookup ::
     RMap.Name (S.Core sym exp pred a)
  -> Map (Wired exp pred)
  -> Wire exp pred a
lookup r =
  do s <- CMR.ask
     return $ case RMap.lookup r s of
       Just (Wired _ (Just w)  _) -> w
       Just (Wired _ (Nothing) _) -> error $
         "Channels.lookup: node " ++ show (hashStableName r) ++ " not wired"
       Nothing -> error $
         "Channels.lookup: failed to find node " ++ show (hashStableName r)

--------------------------------------------------------------------------------

-- | Translate a linked node into a wired one.
wireNode :: forall exp pred a .
     Map (Wired exp pred)
  -> Core Link exp pred a
  -> Core Wire exp pred a
wireNode wires core = case core of
    (S.Var d)     -> S.Var d
    (S.Val e)     -> S.Val e
    (S.Map f s)   -> S.Map f (loadLink s)
    (S.Delay e s) -> S.Delay e (loadLink s)
  where
    loadLink :: Link exp pred b -> Wire exp pred b
    loadLink (Link bundle) = loadBundle bundle

    loadBundle :: Names.Bundle (S.Core Symbol exp pred b) -> Wire exp pred b
    loadBundle (Names.One n)    = loadName n
    loadBundle (Names.Pair a b) = joinWires (loadBundle a) (loadBundle b)

    loadName :: Names.Name (S.Core sym exp pred b) -> Wire exp pred b
    loadName (Names.Name n)  = lookup n wires
    loadName (Names.Fst n)   = leftWire (loadName n)
    loadName (Names.Snd n)   = rightWire (loadName n)
    loadName (Names.Other n) = bufferedWire (loadName n)

--------------------------------------------------------------------------------

-- | Completely wired node, as opposed to 'Wired' which may still have holes.
data WiredNode exp pred a
  where
    WiredNode ::
         Core Wire exp pred a
      -> Wire exp pred a
      -> WiredNode exp pred (Core Symbol exp pred a)

-- | Short-hand for a mapping over wired nodes.
type Wires exp pred = Map (WiredNode exp pred)

-- | Given an incomplete wiring of signal and variable nodes, produces a complete
--   wiring of their combined nodes.
wire :: Map (Wired exp pred) -> Map (Wired exp pred) -> Wires exp pred
wire ss vs = let m = RMap.union ss vs in RMap.hmap (go m) m
  where
    go :: RMap.Map (Wired exp pred) -> Wired exp pred a -> WiredNode exp pred a
    go wires (Wired _ (Just o) core) = WiredNode (wireNode wires core) o

--------------------------------------------------------------------------------
