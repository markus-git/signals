{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

module Signal.Compiler.Backend.C.Buffer where

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

import Data.Typeable hiding (TypeRep)
import Type.Reflection
import Data.Dynamic
import Data.Maybe (isJust)
import Data.List (deleteBy)
import Data.Int
import Data.Ref
import Data.Ref.Map (Map, Entry, Name)
import Data.IntMap (IntMap)
import qualified Data.Ref.Map as RMap
import qualified Data.IntMap as IMap

import System.Mem.StableName (eqStableName) -- !
import Unsafe.Coerce -- !!!

import qualified Language.Embedded.Imperative as C

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | Short-hand for node mappings that only contains delay nodes.
type Delays exp pred = Links exp pred

-- | Short-hand for delay node entry in a mapping.
type Delay exp pred = RMap.Entry (LinkedNode exp pred)

-- | Filter out delays from other linked nodes.
filterDelays :: Links exp pred -> Delays exp pred
filterDelays = RMap.filter delay
  where
    delay :: LinkedNode exp pred a -> Bool
    delay (LinkedNode (S.Delay _ _) _) = True
    delay _ = False

-- | Filter out those delays that do not refer to other delays.
findRoots :: Delays exp pred -> [Delay exp pred]
findRoots ds = RMap.toList $ RMap.filter root ds
  where
    root :: LinkedNode exp pred a -> Bool
    root (LinkedNode (S.Delay _ (Link (Names.One name))) _) =
      not $ lookupName name

    lookupName :: Names.Name (S.Core sym exp pred b) -> Bool
    lookupName (Names.Name n)  = isJust $ RMap.lookup n ds
    lookupName (Names.Fst n)   = lookupName n
    lookupName (Names.Snd n)   = lookupName n
    lookupName (Names.Other n) = lookupName n

-- | Finds the chain of delays, if any, connected to a 'root' delay. Note that
--   searching for delays in this way is pretty slow, and I'll improve it later
--   if needed.
findChain :: Delay exp pred -> Delays exp pred -> [Delay exp pred]
findChain root delays = concat $ searchChain root $ RMap.toList delays
  where
    searchChain :: Delay exp pred -> [Delay exp pred] -> [[Delay exp pred]]
    searchChain d@(RMap.Entry _ (LinkedNode _ (Link (Names.One name)))) delays =
      let nextNodes  = searchName name delays
          nextDelays = removeEntry nextNodes delays
       in map (d:) $ concatMap (flip searchChain nextDelays) nextNodes
      where
        removeEntry :: [RMap.Entry f] -> [RMap.Entry f] -> [RMap.Entry f]
        removeEntry rem list = foldr (deleteBy matchEntry) list rem

        matchEntry :: RMap.Entry f -> RMap.Entry f -> Bool
        matchEntry (RMap.Entry n _) (RMap.Entry m _) = eqStableName n m
    
    searchName :: Names.Name b -> [Delay exp pred] -> [Delay exp pred]
    searchName name = filter (matchEntry name)
      where
        matchEntry :: Names.Name b -> Delay exp pred -> Bool
        matchEntry name (RMap.Entry _ node) = matchName name node

        matchName :: Names.Name b -> LinkedNode exp pred c -> Bool
        matchName name (LinkedNode (S.Delay _ (Link (Names.One m))) _) =
          Names.eqName name m

-- | Find the chains of dealy nodes in a mapping.
findChains :: Links exp pred -> [[Delay exp pred]]
findChains links =
  let delays = filterDelays links
      roots  = findRoots    delays
      chains = map (flip findChain delays) roots
   in filter ((1<) . length) chains

--------------------------------------------------------------------------------

-- | ...
data Buffer pred a
  where
    Buffer :: pred a
      => C.Ref Int32   -- head
      -> C.Ref Int32   -- length
      -> C.Arr Int32 a -- contents
      -> Buffer pred a

declareChains :: [[Delay exp pred]] -> Program instr (Param2 exp pred) ()
declareChains delays = undefined
  where
    declareChain :: [Delay exp pred] -> Program instr (Param2 exp pred) ()
    declareChain = undefined

--------------------------------------------------------------------------------

-- | ...
data Bundle pred a
  where
    -- ^ A single channel.
    Var :: pred a
      => C.Ref a
      -> Bundle pred (Core Symbol exp pred (Identity a))
    -- ^ A single, but delayed channel.
    Buff :: pred a
      => C.Ref a -- rin
      -> C.Ref a -- r
      -> Bundle pred (Core Symbol exp pred (Identity a))
    -- ^ A collection of delayed channels.
    Arr :: pred a
      => C.Ref Int32   -- offset
      -> Buffer pred a -- buffer
      -> Bundle pred (Core Symbol exp pred (Identity a))
    -- ^ A pair of channels.
    Pair ::
         Bundle pred (Core Symbol exp pred a)
      -> Bundle pred (Core Symbol exp pred b)
      -> Bundle pred (Core Symbol exp pred (a, b))

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

data Wire exp pred a
  where
    Wire :: Bundle pred (Core Symbol exp pred a) -> Wire exp pred a

data Wired exp pred a
  where
    Wired ::
         Maybe (Core Wire exp pred a) -- ^ Symbol with incoming channels.
      -> Maybe (Wire exp pred a)      -- ^ Outgoing channels for symbol.
      -> Core Link exp pred a         -- ^ Symbol being wired.
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
bufferedWire (Wire (Buff rin r)) = Wire (Var r)

-- | Get the name of a bundle.
nameOf :: Names.Bundle (Core Symbol exp pred (Identity a)) -> String
nameOf (Names.One n) = Names.toString n

-- | Get the buffered name of bundle.
otherOf :: Names.Bundle (Core Symbol exp pred (Identity a)) -> String
otherOf (Names.One n) = Names.toString $ Names.other n

--------------------------------------------------------------------------------

type Input = Hide (C.Ref)

input :: C.Ref a -> Input
input = Hide

-- | Fetch the corresponding input variable for a dynamic object.
variableOf :: forall a . Proxy (Identity a) -> Dynamic -> [Input] -> C.Ref a
variableOf _ dyn is = go (indexOf dyn) is
  where
    go :: Int -> [Input] -> C.Ref a
    go 0 _  = error "signals.variables: access to input from constant signal."
    go i is = case is !! (i - 1) of
      Hide ref -> unsafeCoerce ref

-- | Check the arity of a dynamic object.
indexOf :: Dynamic -> Int
indexOf (Dynamic trep _) = go trep
  where
    go :: TypeRep a -> Int
    go (Con _)       = 0
    go (App _ _)     = 0
    go (Fun arg res) = 1 + go arg

--------------------------------------------------------------------------------

declareVariableNodes :: forall instr exp pred .
     (C.RefCMD :<: instr, C.ArrCMD :<: instr)
  => RMap.Entry (LinkedNode exp pred)
  -> [Input]
  -> Program instr (Param2 exp pred) (Maybe (RMap.Entry (Wired exp pred)))
declareVariableNodes (RMap.Entry name (LinkedNode core (Link link))) is =
  let entry w = RMap.Entry name (Wired Nothing (Just (Wire w)) core) in
  case core of
    S.Var   d   -> do
      return $ Just $ entry $ Var $ variableOf Proxy d is
    S.Val   e   -> do
      ref <- C.initNamedRef (nameOf link) e
      return $ Just $ entry $ Var ref
    S.Map   _ _ -> do
      refs <- declareVariableBundle witness link
      return $ Just $ entry refs
    S.Delay e _ -> do
      let n = nameOf  link
      let o = otherOf link
      rin <- C.newNamedRef n
      r   <- C.initNamedRef o e
      return $ Just $ entry $ Buff rin r
    _ -> return Nothing

declareVariableBundle ::
     (C.RefCMD :<: instr, C.ArrCMD :<: instr)
  => TupleRep pred a
  -> Names.Bundle (S.Core Symbol exp pred a)
  -> Program instr (Param2 exp pred) (Bundle pred (S.Core Symbol exp pred a))
declareVariableBundle (Single) (Names.One n) =
  do ref <- C.newNamedRef $ Names.toString n
     return $ Var ref
declareVariableBundle (Tuple l r) (Names.Pair n m) =
  do vl <- declareVariableBundle l n
     vr <- declareVariableBundle r m
     return $ Pair vl vr

--------------------------------------------------------------------------------
