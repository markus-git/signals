{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Signal.Compiler where

import Signal.Core (Core, Symbol)
import Signal.Core.Reify (reifyF1, Key(..))
import Signal.Core.Frontend (Sig(..))
import Signal.Core.Data (Hide(..))
import qualified Signal.Core as Core

import Signal.Compiler.Cycles
import Signal.Compiler.Sorter
import Signal.Compiler.Linker
import Signal.Compiler.Linker.Names
import qualified Signal.Compiler.Linker.Names as Names

-- debug
import Signal.Compiler.Backend.VHDL.Channels
import qualified Signal.Compiler.Backend.VHDL.Channels as Channels
import qualified Language.Embedded.Hardware.Command.CMD as H
--

import Data.Typeable
import Data.Ref
import Data.Ref.Map (Name, Map)
import qualified Data.Ref.Map as RMap

import System.Mem.StableName -- temp!
import Data.Hashable -- temp!

import Prelude
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Compilation
--------------------------------------------------------------------------------

debugFun ::
     (Typeable exp, Typeable pred, Typeable a, Typeable b, pred a, pred b)
  => (Sig exp pred a -> Sig exp pred b) -> IO ()
debugFun f =
  do (root, nodes) <- reifyF1 (runSig . f . Sig)
     let order = sorter root nodes
         cycle = cycles root nodes
         links = linker root nodes
     case cycle of
       True  -> error "signal compiler: found cycle"
       False -> putStrLn $
         "order: " ++ show order ++ "\n" ++
         "links: " ++ show links ++ "\n"

--------------------------------------------------------------------------------
         
instance Show (Map (LinkedNode exp pred))
  where
    show = show . RMap.toList

instance Show (RMap.Entry (LinkedNode exp pred))
  where
    show (RMap.Entry name node) = show name ++ ":" ++ show node

instance Show (RMap.Name a)
  where
    show name = show $ hashStableName name

instance Show (LinkedNode exp pred a)
  where
    show (LinkedNode core out) = "(" ++ show out ++ ")" ++ show core

instance Show (Core Link exp pred a)
  where
    show (Core.Val _) = "L.Val"
    show (Core.Map _ a) = "L.Map " ++ show a
    show (Core.Pair l r) = "L.Pair " ++ show l ++ " " ++ show r
    show (Core.Fst p) = "L.Fst " ++ show p
    show (Core.Snd p) = "L.Snd " ++ show p
    show (Core.Delay _ a) = "L.Delay " ++ show a
    show (Core.Var _) = "L.Var"

instance Show (Link exp pred a)
  where
    show (Link b) = show b

instance Show (Names.Bundle (Core Symbol exp pred a))
  where
    show (Names.One n) = show n
    show (Names.Pair l r) = "(" ++ show l ++ "," ++ show r ++ ")"

instance Show (Names.Name (Core Symbol exp pred a))
  where
    show (Names.Name n) = show n
    show (Names.Fst p)  = "(left " ++ show p ++ ")"
    show (Names.Snd p)  = "(right " ++ show p ++ ")"

instance Show (Hide (Key exp pred))
  where
    show (Hide k) = show k

instance Show (Key exp pred a)
  where
    show (Key n) = show n

instance Show (Hide (WiredNode exp pred))
  where
    show (Hide k) = show k

instance Show (WiredNode exp pred a)
  where
    show (WiredNode core out) = "(" ++ show out ++ ")" ++ show core

instance Show (Core Wire exp pred a)
  where
    show (Core.Val _) = "W.Val"
    show (Core.Map _ a) = "W.Map " ++ show a
    show (Core.Pair l r) = "W.Pair " ++ show l ++ " " ++ show r
    show (Core.Fst p) = "W.Fst " ++ show p
    show (Core.Snd p) = "W.Snd " ++ show p
    show (Core.Delay _ a) = "W.Delay " ++ show a
    show (Core.Var _) = "W.Var"

instance Show (Wire exp pred a)
  where
    show (Wire b) = show b

instance Show (Channels.Bundle pred a)
  where
    show (Channels.Chan c) = show c
    show (Channels.Buff rin r) = show rin ++ "=>" ++ show r
    show (Channels.Pair l r) = show l ++ "<>" ++ show r

instance Show (Channels.Channel pred a)
  where
    show (Channels.S sig) = case sig of
      (H.SignalC id) -> "Signal " ++ show id
      (H.SignalE _)  -> "Signal X"
    show (Channels.V var) = case var of
      (H.VariableC id) -> "Variable " ++ show id
      (H.VariableE _)  -> "Variable X"

instance Show (Channels.Wires exp pred)
  where
    show = show . RMap.toList

instance Show (RMap.Entry (WiredNode exp pred))
  where
    show (RMap.Entry name wires) = show name ++ ";" ++ show wires

instance Show (RMap.Map (Channels.Wired exp pred))
  where
    show = show . RMap.toList

instance Show (RMap.Entry (Wired exp pred))
  where
    show (RMap.Entry name wired) = show name ++ ":" ++ show wired

instance Show (Wired exp pred a)
  where
    show (Wired inc out core) = "(" ++ show inc ++ ")(" ++ show out ++ ")" ++ show core

--------------------------------------------------------------------------------
