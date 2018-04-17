{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Signal.Compiler where

import Signal.Core
import Signal.Core.Stream
import Signal.Core.Reify
import Signal.Core.Witness
import Signal.Core.Frontend

import Signal.Compiler.Cycles
import Signal.Compiler.Sorter
import Signal.Compiler.Linker
import Signal.Compiler.Linker.Names

import Data.Typeable
import Data.Ref
import Data.Ref.Map (Name, Map)
import qualified Data.Ref.Map as RMap

import Prelude
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Compilation
--------------------------------------------------------------------------------

-- | Compile signals into streams.
compile :: pred a => Sig exp pred a -> IO (Str instr exp pred a)
compile f =
  do (root, nodes) <- reify (runSig f)
     let order = sorter root nodes
         cycle = cycles root nodes
         links = linker root nodes
     return $ case cycle of
       True  -> error "ERR: found cycle"
       False -> error "OK"

compileFun :: (Typeable exp, Typeable pred, Typeable a, Typeable b, pred a, pred b)
  =>    (Sig       exp pred a -> Sig       exp pred b)
  -> IO (Str instr exp pred a -> Str instr exp pred b)
compileFun f =
  do (root, nodes) <- reifyFun (runSig . f . Sig)
     let order = sorter root nodes
         cycle = cycles root nodes
         links = linker root nodes
     return $ case cycle of
       True  -> error "signal compiler: found cycle"
       False -> error "OK"

--------------------------------------------------------------------------------
