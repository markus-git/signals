{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE UndecidableInstances #-}

module Signal.Compiler.Backend.C where

import Signal.Core (Core, Symbol, Expr)
import Signal.Core.Reify (reify, reifyF1, reifyF2, Key)
import Signal.Core.Witness
import Signal.Core.Frontend (Sig(..))
import Signal.Core.Data
import qualified Signal.Core as S
import qualified Signal.Core.Stream as S
import qualified Signal.Core.Reify as S

import Signal.Compiler.Cycles
import Signal.Compiler.Sorter
import Signal.Compiler.Linker

import Signal.Compiler.Backend.C.Buffers hiding (Delay)

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

import qualified Language.Embedded.Imperative as C
import qualified Language.Embedded.Expression as C

import Prelude hiding (Ordering)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------
{-
instance (C.FreeExp exp, S.Subsume pred (C.FreePred exp)) => S.Literal exp pred
  where
    lit = literal

literal :: forall exp pred a . (C.FreeExp exp, S.Subsume pred (C.FreePred exp))
  => Dict (pred a)
  -> a
  -> exp a
literal dict a = case S.wit (dict :: Dict (pred a)) of
  (Dict :: Dict (C.FreePred exp a)) -> C.constExp a
-}
--------------------------------------------------------------------------------

readChan :: forall instr exp pred a .
  (C.RefCMD :<: instr, C.FreeExp exp, S.Subsume pred (C.FreePred exp), pred a)
  => C.Ref a -> Program instr (Param2 exp pred) (exp a)
readChan ref = case S.wit (Dict :: Dict (pred a)) of
  (Dict :: Dict (C.FreePred exp a)) -> C.getRef ref

writeChan :: (C.RefCMD :<: instr, pred a)
  => C.Ref a -> exp a -> Program instr (Param2 exp pred) ()
writeChan ref e = C.setRef ref e

--------------------------------------------------------------------------------

-- | Read a nested expression of a wire.
readWire ::
     ( C.RefCMD :<: instr
     , C.FreeExp exp
     , S.Subsume pred (C.FreePred exp)
     )
  => Wire exp pred a
  -> Program instr (Param2 exp pred) (Expr exp a)
readWire (Wire b) = readBundle b
  where
    readBundle ::
         ( C.RefCMD :<: instr
         , C.FreeExp exp
         , S.Subsume pred (C.FreePred exp)
         )
      => Bundle pred (Core Symbol exp pred a)
      -> Program instr (Param2 exp pred) (Expr exp a)
    readBundle (Var c)    = readChan c
    readBundle (Buff _ r) = readChan r
    readBundle (Pair a b) = (,) <$> readBundle a <*> readBundle b

-- | Write a nested expression to a wire.
writeWire ::
     ( C.RefCMD :<: instr
     , C.FreeExp exp
     , S.Subsume pred (C.FreePred exp)
     )
  => Wire exp pred a
  -> Expr exp a
  -> Program instr (Param2 exp pred) ()
writeWire (Wire b) e = writeBundle b e
  where
    writeBundle ::
         ( C.RefCMD :<: instr
         , C.FreeExp exp
         , S.Subsume pred (C.FreePred exp)
         )
      => Bundle pred (Core Symbol exp pred a)
      -> Expr exp a
      -> Program instr (Param2 exp pred) ()
    writeBundle (Var c)      e      = writeChan c e
    writeBundle (Buff rin _) e      = writeChan rin e
    writeBundle (Pair a b)   (l, r) = writeBundle a l >> writeBundle b r

--------------------------------------------------------------------------------

compileSymbol ::
     ( C.RefCMD :<: instr
     , C.FreeExp exp
     , S.Subsume pred (C.FreePred exp)
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
     ( C.RefCMD :<: instr
     , C.FreeExp exp
     , S.Subsume pred (C.FreePred exp)
     )
  => WiredNode exp pred a
  -> Program instr (Param2 exp pred) ()
updateSymbol (WiredNode (S.Delay _ _) (Wire (Buff rin r))) =
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

data Str instr exp pred
  where
    Str :: pred a
      => S.Stream instr exp pred (exp a)
      -> Str instr exp pred

data Source instr exp pred
  where
    Source :: pred a
      => Program instr (Param2 exp pred) (exp a)
      -> C.Ref a
      -> Source instr exp pred

sourceRef :: Source instr exp pred -> Input
sourceRef (Source _ r) = input r

--------------------------------------------------------------------------------

compileCore :: forall instr exp pred a .
  ( C.RefCMD :<: instr
  , C.ArrCMD :<: instr
  , C.FreeExp exp
  , S.Subsume pred (C.FreePred exp)
  )
  => Key exp pred (Identity a)
  -> [Str instr exp pred]
  -> Ordering exp pred
  -> Links exp pred
  -> Program instr (Param2 exp pred) (Program instr (Param2 exp pred) (exp a))
compileCore root inputs order links =
  do sources   <- mapM initSource inputs
     variables <- declareVariables links $ map sourceRef sources
     return $
       do let wires = wire variables
          let (nodes, delays) = sortSymbols order wires
          -- 1. Read inputs.
          mapM_ updateSource sources
          -- 2. Update signal nodes.
          run nodes
          -- 3. Write combinatorial nodes.
          run delays
          -- 4. Write delay nodes.
          update delays
          -- 5. Update delays.
          output root wires
  where
    initSource :: Str instr exp pred
      -> Program instr (Param2 exp pred) (Source instr exp pred)
    initSource (Str (S.Stream init)) =
      do prg <- init
         ref <- C.newRef
         return $ Source prg ref

    updateSource :: Source instr exp pred
      -> Program instr (Param2 exp pred) ()
    updateSource (Source prg ref) =
      do val <- prg
         C.setRef ref val

    run :: [Node exp pred] -> Program instr (Param2 exp pred) ()
    run = mapM_ (\(Hide s) -> compileSymbol s)

    update :: [Node exp pred] -> Program instr (Param2 exp pred) ()
    update = mapM_ (\(Hide s) -> updateSymbol s)

    output :: Key exp pred (Identity a) -> Wires exp pred
      -> Program instr (Param2 exp pred) (exp a)
    output (S.Key k) m = case RMap.lookup k m of
        Just (WiredNode _ (Wire (Var    ref))) -> readChan ref
        Just (WiredNode _ (Wire (Buff _ ref))) -> readChan ref
        Nothing -> error "signals.c: couldn't find output node."

--------------------------------------------------------------------------------

compileNodes ::
  ( Typeable exp, Typeable pred, Typeable a
  , C.RefCMD :<: instr
  , C.ArrCMD :<: instr
  , C.FreeExp exp
  , S.Subsume pred (C.FreePred exp)
  , pred a
  )
  => S.Key exp pred (Identity a)
  -> [Str instr exp pred]
  -> S.Nodes exp pred
  -> Program instr (Param2 exp pred) (Program instr (Param2 exp pred) (exp a))
compileNodes root inputs nodes =
  let order = sorter root nodes
      cycle = cycles root nodes
      links = linker root nodes
  in case cycle of
    True  -> error "signal compiler: found cycle"
    False -> compileCore root inputs order links

--------------------------------------------------------------------------------

compile ::
  ( Typeable exp, Typeable pred, Typeable a
  , C.RefCMD :<: instr
  , C.ArrCMD :<: instr
  , C.FreeExp exp
  , S.Subsume pred (C.FreePred exp)
  , pred a
  )
  => Sig exp pred a
  -> IO (S.Str instr exp pred a)
compile sig =
  do (root, nodes) <- reify (runSig sig)
     return $
       S.Stream $ compileNodes root [] nodes

--------------------------------------------------------------------------------

compileF1 ::
  ( Typeable exp, Typeable pred, Typeable a, Typeable b
  , C.RefCMD :<: instr
  , C.ArrCMD :<: instr
  , C.FreeExp exp
  , S.Subsume pred (C.FreePred exp)
  , pred a, pred b
  )
  => (Sig exp pred a -> Sig exp pred b)
  -> IO (S.Str instr exp pred a -> S.Str instr exp pred b)
compileF1 f =
  do (root, nodes) <- reifyF1 (runSig . f . Sig)
     return $ \str ->
       S.Stream $ compileNodes root [Str str] nodes

--------------------------------------------------------------------------------

compileF2 ::
  ( Typeable exp, Typeable pred, Typeable a, Typeable b, Typeable c
  , C.RefCMD :<: instr
  , C.ArrCMD :<: instr
  , C.FreeExp exp
  , S.Subsume pred (C.FreePred exp)
  , pred a, pred b, pred c
  )
  => (Sig exp pred a -> Sig exp pred b -> Sig exp pred c)
  -> IO (   S.Str instr exp pred a
         -> S.Str instr exp pred b
         -> S.Str instr exp pred c)
compileF2 f =
  do (root, nodes) <- reifyF2 (\a b -> runSig $ f (Sig a) (Sig b))
     return $ \str1 -> \str2 ->
       S.Stream $ compileNodes root [Str str1, Str str2] nodes

--------------------------------------------------------------------------------
