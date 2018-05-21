{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE UndecidableInstances #-}

module Signal.Compiler.Backend.VHDL where

import Signal.Core (Core, Symbol, Expr)
import Signal.Core.Stream (Stream, Str)
import Signal.Core.Reify (reify, reifyF1, reifyF2, Key)
import Signal.Core.Witness
import Signal.Core.Frontend (Sig(..))
import Signal.Core.Data
import qualified Signal.Core as S
import qualified Signal.Core.Reify as S

import Signal.Compiler.Cycles
import Signal.Compiler.Sorter
import Signal.Compiler.Linker

import Signal.Compiler.Backend.VHDL.Channels

import Control.Monad.Identity (Identity)
import Control.Monad.Reader   (ReaderT)
import Control.Monad.Operational.Higher
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.Reader   as CMR

import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import Data.Constraint
import Data.Typeable
import Data.Ref
import Data.Ref.Map (Name, Map)
import qualified Data.Ref.Map as RMap

-- hardware-edsl.
import qualified Language.Embedded.Hardware.Interface as HDL
import qualified Language.Embedded.Hardware.Expression.Represent as HDL
import qualified Language.Embedded.Hardware.Command as HDL
import qualified Language.Embedded.Hardware.Command.Frontend as HDL

import Prelude hiding (read, Ordering)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Compilation
--------------------------------------------------------------------------------  
{-
instance (HDL.FreeExp exp, S.Subsume pred (HDL.PredicateExp exp))
    => S.Literal exp pred
  where
    lit = literal

literal :: forall exp pred a . (HDL.FreeExp exp, S.Subsume pred (HDL.PredicateExp exp))
  => Dict (pred a)
  -> a
  -> exp a
literal dict a = case S.wit (dict :: Dict (pred a)) of
  (Dict :: Dict (HDL.PredicateExp exp a)) -> HDL.litE a
-}
--------------------------------------------------------------------------------

readChan :: forall instr exp pred a .
     ( HDL.SignalCMD   :<: instr
     , HDL.VariableCMD :<: instr
     , HDL.FreeExp exp
     , S.Subsume pred (HDL.PredicateExp exp)
     )
  => Channel pred a
  -> Program instr (Param2 exp pred) (exp a)
readChan (S s) = case S.wit (Dict :: Dict (pred a)) of
  (Dict :: Dict (HDL.PredicateExp exp a)) -> HDL.getSignal s
readChan (V v) = case S.wit (Dict :: Dict (pred a)) of
  (Dict :: Dict (HDL.PredicateExp exp a)) -> HDL.getVariable v

writeChan ::
     ( HDL.SignalCMD   :<: instr
     , HDL.VariableCMD :<: instr
     )
  => Channel pred a
  -> exp a
  -> Program instr (Param2 exp pred) ()
writeChan (S s) e = HDL.setSignal   s e
writeChan (V v) e = HDL.setVariable v e

--------------------------------------------------------------------------------

-- | Read a nested expression of a wire.
readWire ::
     ( HDL.SignalCMD   :<: instr
     , HDL.VariableCMD :<: instr
     , HDL.FreeExp exp
     , S.Subsume pred (HDL.PredicateExp exp)
     )
  => Wire exp pred a
  -> Program instr (Param2 exp pred) (Expr exp a)
readWire (Wire b) = readBundle b
  where
    readBundle ::
         ( HDL.SignalCMD   :<: instr
         , HDL.VariableCMD :<: instr
         , HDL.FreeExp exp
         , S.Subsume pred (HDL.PredicateExp exp)
         )
      => Bundle pred (Core Symbol exp pred a)
      -> Program instr (Param2 exp pred) (Expr exp a)
    readBundle (Chan c)   = readChan c
    readBundle (Buff _ r) = readChan r
    readBundle (Pair a b) = (,) <$> readBundle a <*> readBundle b

-- | Write a nested expression to a wire.
writeWire ::
     ( HDL.SignalCMD   :<: instr
     , HDL.VariableCMD :<: instr
     , HDL.FreeExp exp
     , S.Subsume pred (HDL.PredicateExp exp)
     )
  => Wire exp pred a
  -> Expr exp a
  -> Program instr (Param2 exp pred) ()
writeWire (Wire b) e = writeBundle b e
  where
    writeBundle ::
         ( HDL.SignalCMD   :<: instr
         , HDL.VariableCMD :<: instr
         , HDL.FreeExp exp
         , S.Subsume pred (HDL.PredicateExp exp)
         )
      => Bundle pred (Core Symbol exp pred a)
      -> Expr exp a
      -> Program instr (Param2 exp pred) ()
    writeBundle (Chan c)     e      = writeChan c e
    writeBundle (Buff rin _) e      = writeChan rin e
    writeBundle (Pair a b)   (l, r) = writeBundle a l >> writeBundle b r

--------------------------------------------------------------------------------

compileSymbol ::
     ( HDL.SignalCMD   :<: instr
     , HDL.VariableCMD :<: instr
     , HDL.FreeExp exp
     , S.Subsume pred (HDL.PredicateExp exp)
     )
  => WiredNode exp pred a
  -> Program instr (Param2 exp pred) ()
compileSymbol (WiredNode sym out) = case sym of
  -- Constant nodes are initialized as such, so there's no need to update their
  -- values by writing or reading.
  (S.Val _) -> return ()
  -- Function nodes reads their input, modifies it using their function, and
  -- then outputs the result.
  (S.Map f w) ->
    do inp <- readWire w
       writeWire out (f inp)
  -- Write to and reading from delayed nodes usually requires a bit of extra
  -- care, as they're buffered. However, the write and read functions for wires
  -- already handles this, and the buffering is handled seperatly, so we can
  -- read and write here as usual.
  (S.Delay d w) ->
    do inp <- readWire w
       writeWire out inp
  -- Reading from a variable node means reading from the input.
  (S.Var _) ->
    do return ()
  -- Other nodes should have been filtered out already.
  _ -> return ()

--------------------------------------------------------------------------------

-- | Update a delayed node.
updateSymbol ::
     ( HDL.SignalCMD   :<: instr
     , HDL.VariableCMD :<: instr
     , HDL.FreeExp exp
     , S.Subsume pred (HDL.PredicateExp exp)
     )
  => Wired exp pred a
  -> Program instr (Param2 exp pred) ()
updateSymbol (Wired Nothing (Just (Wire (Buff rin r))) (S.Delay _ _)) =
  readChan rin >>= writeChan r
updateSymbol _ =
  return ()

--------------------------------------------------------------------------------

type Node  exp pred = Hide (WiredNode exp pred)
type Delay exp pred = Node exp pred

-- | Looks up all the nodes in order, filters out any nodes used soley for
--   managing tuples, and seperates the delays.
sortSymbols ::
     Ordering exp pred
  -> Wires exp pred
  -> ([Delay exp pred], [Node exp pred])
sortSymbols order wires =
      partitionEithers
    $ map sort
    $ catMaybes
    $ map (flip lookup wires) order
  where
    sort :: Node exp pred -> Either (Node exp pred) (Node exp pred)
    sort h@(Hide node) =
      if isDelay node
        then Left h
        else Right h
    
    lookup :: Hide (S.Key exp pred) -> Wires exp pred -> Maybe (Node exp pred)
    lookup (Hide (S.Key name)) wires = case RMap.lookup name wires of
      Nothing   -> Nothing
      Just node ->
        if isConnector node
          then Nothing
          else Just $ Hide node

    isDelay :: WiredNode exp pred a -> Bool
    isDelay (WiredNode core _) = case core of
      (S.Delay {}) -> True
      _            -> False

    isConnector :: WiredNode exp pred a -> Bool
    isConnector (WiredNode core _) = case core of
      (S.Pair {}) -> True
      (S.Fst  {}) -> True
      (S.Snd  {}) -> True
      _           -> False

--------------------------------------------------------------------------------
-- todo: Find a way of reading from input and writing to output that doesn't
--       involve coercion. Problem is that creation and linking of core nodes is
--       seperated from the creation of inputs and outputs for the component
--       below. I do get a reference to the root node, which is useful for
--       linking with the output, but the input problem remains.
--------------------------------------------------------------------------------

compileCore :: forall instr exp pred a .
     ( HDL.SignalCMD    :<: instr
     , HDL.VariableCMD  :<: instr
     , HDL.ComponentCMD :<: instr
     , HDL.ProcessCMD   :<: instr
     , HDL.FreeExp exp
     , S.Subsume pred (HDL.PredicateExp exp)
     , HDL.PrimType a, pred a
     )
  => Key exp pred (Identity a)
  -> [Input]
  -> HDL.Signal a
  -> Ordering exp pred
  -> Links exp pred
  -> Program instr (Param2 exp pred) ()
compileCore root inputs output order links =
  do -- First of, we declare all the linked signals.
     signals <- declareSignals links inputs
     -- Then, we create the two processes design.
     -- > Combinatorial:
     HDL.process (foldr (\(Hide i) is -> i HDL..: is) [] inputs) $
       do -- 1. Declare local variables.
          variables <- declareVariables links
          -- 2. Put together signal and variable wires.
          let wires = wire signals variables
          -- 3. Seperate delays.
          let (delays, nodes) = sortSymbols order wires
          -- 4. Put in the combinatorial nodes.
          run nodes
          -- 5. Put in any writes to delayed nodes.
          run delays
          -- 6. Write output.
          writeOutput root output wires
     -- > Sequential:
     HDL.process [] $
       do -- 1. No local variables to declare or wiring to do, as we are only
          --    interested in updating the outputs of any delayed signals.
          --    Sorting doesn't matter, as each delay only interacts with
          --    itself when updating.
          update signals
  where
    run :: [Node exp pred] -> Program instr (Param2 exp pred) ()
    run = mapM_ (\(Hide s) -> compileSymbol s)
    
    update :: Map (Wired exp pred) -> Program instr (Param2 exp pred) ()
    update = mapM_ (\(RMap.Entry _ s) -> updateSymbol s) . RMap.toList

    writeOutput ::
         Key exp pred (Identity a)
      -> HDL.Signal a
      -> Wires exp pred
      -> Program instr (Param2 exp pred) ()
    writeOutput (S.Key k) out m = case RMap.lookup k m of
        Just (WiredNode _ (Wire (Chan   o))) -> writeOutputChan o
        Just (WiredNode _ (Wire (Buff _ o))) -> writeOutputChan o
        Nothing -> error "signals.vhdl: couldn't find output node."
      where
        writeOutputChan :: Channel pred a -> Program instr (Param2 exp pred) ()
        writeOutputChan c =
          do val <- readChan c
             HDL.setSignal out val
    
--------------------------------------------------------------------------------

compileNodes :: 
       -- Reification requires that our types be 'Typeable'.
     ( Typeable exp, Typeable pred, Typeable a
       -- Compilation requires that the following instructions are supported.
     , HDL.SignalCMD    :<: instr
     , HDL.VariableCMD  :<: instr
     , HDL.ComponentCMD :<: instr
     , HDL.ProcessCMD   :<: instr
       -- As both variables and signals are declared, the expression and
       -- predicate types are required to support such actions.
     , HDL.FreeExp exp
     , S.Subsume pred (HDL.PredicateExp exp)
       -- The output signal must be well-typed.
     , HDL.PrimType a, pred a
     )
  => S.Key exp pred (Identity a)
  -> [Input]
  -> HDL.Signal a
  -> S.Nodes exp pred
  -> Program instr (Param2 exp pred) ()
compileNodes root inputs output nodes =
  let order = sorter root nodes
      cycle = cycles root nodes
      links = linker root nodes
  in case cycle of
    True  -> error "signal compiler: found cycle"
    False -> compileCore root inputs output order links

--------------------------------------------------------------------------------

compile ::
       -- Reification requires that our types be 'Typeable'.
     ( Typeable exp, Typeable pred
       -- Compilation requires that the following instructions are supported.
     , HDL.SignalCMD    :<: instr
     , HDL.VariableCMD  :<: instr
     , HDL.ComponentCMD :<: instr
     , HDL.ProcessCMD   :<: instr
       -- As both variables and signals are declared, the expression and
       -- predicate types are required to support such actions.
     , HDL.FreeExp exp
     , S.Subsume pred (HDL.PredicateExp exp)
       -- The output signal must be well-typed.
     , HDL.PrimType a, Integral a, pred a
       -- The clock signal requires that booleans be supported.
     , pred Bool
     )
  => Sig exp pred a
  -> IO (HDL.Sig instr exp pred Identity (HDL.Signal a -> ()))
compile sig =
  do (root, nodes) <- reify (runSig sig)
     return $ HDL.output $ \out -> HDL.ret $
       compileNodes root [] out nodes

--------------------------------------------------------------------------------

compileF1 ::
     ( Typeable exp, Typeable pred, Typeable a, Typeable b
     , HDL.SignalCMD    :<: instr
     , HDL.VariableCMD  :<: instr
     , HDL.ComponentCMD :<: instr
     , HDL.ProcessCMD   :<: instr
     , HDL.FreeExp exp
     , S.Subsume pred (HDL.PredicateExp exp)
     , HDL.PrimType a, Integral a, pred a
     , HDL.PrimType b, Integral b, pred b
     )
  => (Sig exp pred a -> Sig exp pred b)
  -> IO (HDL.Sig instr exp pred Identity (HDL.Signal a -> HDL.Signal b -> ()))
compileF1 f =
  do (root, nodes) <- reifyF1 (runSig . f . Sig)
     return $ HDL.input  $ \inc -> HDL.output $ \out -> HDL.ret $
       compileNodes root [input inc] out nodes

--------------------------------------------------------------------------------

compileF2 ::
     ( Typeable exp, Typeable pred, Typeable a, Typeable b, Typeable c
     , HDL.SignalCMD    :<: instr
     , HDL.VariableCMD  :<: instr
     , HDL.ComponentCMD :<: instr
     , HDL.ProcessCMD   :<: instr
     , HDL.FreeExp exp
     , S.Subsume pred (HDL.PredicateExp exp)
     , HDL.PrimType a, Integral a, pred a
     , HDL.PrimType b, Integral b, pred b
     , HDL.PrimType c, Integral c, pred c
     )
  => (Sig exp pred a -> Sig exp pred b -> Sig exp pred c)
  -> IO (HDL.Sig instr exp pred Identity (HDL.Signal a -> HDL.Signal b -> HDL.Signal c -> ()))
compileF2 f =
  do (root, nodes) <- reifyF2 (\a b -> runSig $ f (Sig a) (Sig b))
     return $ HDL.input $ \incA -> HDL.input $ \incB -> HDL.output $ \out -> HDL.ret $
       compileNodes root [input incA, input incB] out nodes

--------------------------------------------------------------------------------
