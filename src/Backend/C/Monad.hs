-- Copyright (c) 2009-2010
-- The President and Fellows of Harvard College.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
-- notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
-- may be used to endorse or promote products derived from this software
-- without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-- Copyright (c) 2011-2012, Geoffrey Mainland
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without modification,
-- are permitted provided that the following conditions are met:
-- 1. Redistributions of source code must retain the above copyright notice, this
-- list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright notice,
-- this list of conditions and the following disclaimer in the documentation
-- and/or other materials provided with the distribution.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
-- ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- Copyright (c) 2015, Anders Persson
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- * Redistributions of source code must retain the above copyright
-- notice, this list of conditions and the following disclaimer.
-- 
-- * Redistributions in binary form must reproduce the above
-- copyright notice, this list of conditions and the following
-- disclaimer in the documentation and/or other materials provided
-- with the distribution.
-- 
-- * Neither the name of Anders Persson nor the names of other
-- contributors may be used to endorse or promote products derived
-- from this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend.C.Monad where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Exception.Instances
import Data.List

import Language.C.Quote.C
import qualified Language.C.Syntax as C
import qualified Data.Set          as Set

import Text.PrettyPrint.Mainland

data Flags = Flags

data CEnv = CEnv
    { _flags      :: Flags

    , _unique     :: !Integer

    , _includes   :: Set.Set String
    , _typedefs   :: [C.Definition]
    , _prototypes :: [C.Definition]
    , _globals    :: [C.Definition]

    , _params     :: [C.Param]
    , _locals     :: [C.InitGroup]
    , _stms       :: [C.Stm]
    , _finalStms  :: [C.Stm]

    }

defaultCEnv :: Flags -> CEnv
defaultCEnv flags = CEnv
    { _flags      = flags
    , _unique     = 0
    , _includes   = Set.empty
    , _typedefs   = []
    , _prototypes = []
    , _globals    = []
    , _params     = []
    , _locals     = []
    , _stms       = []
    , _finalStms  = []
    }

newtype C a = C { unC :: StateT CEnv (ExceptionT IO) a }
  deriving (Functor, Applicative, Monad, MonadException, MonadIO, MonadState CEnv)

runC :: C a -> CEnv -> IO (a, CEnv)
runC m s = runExceptionT (runStateT (unC m) s) >>= liftException

fastDefEq :: C.Definition -> C.Definition -> Bool
fastDefEq (C.FuncDef (C.OldFunc _ i _ _ _ _ _) _) (C.FuncDef (C.OldFunc _ j _ _ _ _ _) _) = i==j
fastDefEq _ _ = False

-- | Extract a compilation unit from the 'CEnv' state
cenvToCUnit :: CEnv -> [C.Definition]
cenvToCUnit env =
    [cunit|$edecls:includes
           $edecls:typedefs
           $edecls:prototypes
           $edecls:globals|]
  where
    includes = map toInclude (Set.toList (_includes env))
      where
        toInclude :: String -> C.Definition
        toInclude inc = [cedecl|$esc:("#include " ++ inc)|]
    typedefs   = reverse $ _typedefs env
    prototypes = reverse $ nubBy fastDefEq $ _prototypes env
    globals    = reverse $ nubBy fastDefEq $ _globals env

gensym :: String -> C String
gensym s = do
    u <- gets _unique
    modify $ \s -> s { _unique = u + 1 }
    return $ s ++ show u

addInclude :: String -> C ()
addInclude inc = modify $ \s ->
    s { _includes = Set.insert inc (_includes s) }

addTypedef :: C.Definition -> C ()
addTypedef def = modify $ \s ->
    s { _typedefs = def : _typedefs s }

addPrototype :: C.Definition -> C ()
addPrototype def = modify $ \s ->
    s { _prototypes = def : _prototypes s }

addGlobal :: C.Definition -> C ()
addGlobal def = modify $ \s ->
    s { _globals = def : _globals s }

addParam :: C.Param -> C ()
addParam param = modify $ \s ->
    s { _params = param : _params s }

addLocal :: C.InitGroup -> C ()
addLocal def = modify $ \s ->
    s { _locals = def : _locals s }

addStm :: C.Stm -> C ()
addStm stm = modify $ \s ->
    s { _stms = stm : _stms s }

addFinalStm :: C.Stm -> C ()
addFinalStm stm = modify $ \s ->
    s { _finalStms = stm : _finalStms s }

inBlock :: C a -> C a
inBlock act = do
    (a, items) <- inNewBlock act
    addStm [cstm|{ $items:items }|]
    return a

inNewBlock :: C a -> C (a, [C.BlockItem])
inNewBlock act = do
    oldLocals    <- gets _locals
    oldStms      <- gets _stms
    oldFinalStms <- gets _finalStms
    modify $ \s -> s { _locals = [], _stms = [], _finalStms = [] }
    x <- act
    locals    <- reverse <$> gets _locals
    stms      <- reverse <$> gets _stms
    finalstms <- reverse <$> gets _finalStms
    modify $ \s -> s { _locals    = oldLocals
                     , _stms      = oldStms
                     , _finalStms = oldFinalStms
                     }
    return (x, map C.BlockDecl locals ++
               map C.BlockStm  stms   ++
               map C.BlockStm  finalstms
           )

inNewBlock_ :: C () -> C [C.BlockItem]
inNewBlock_ act = snd <$> inNewBlock act

inNewFunction :: C () -> C ([C.Param],[C.BlockItem])
inNewFunction comp = do
    oldParams <- gets _params
    modify $ \s -> s { _params = [] }
    items  <- inNewBlock_ comp
    params <- gets _params
    modify $ \s -> s { _params = oldParams }
    return (reverse params, items)

inFunction :: String -> C () -> C ()
inFunction fun act = do
    (params,items) <- inNewFunction act
    addPrototype [cedecl| void $id:fun($params:params);|]
    addGlobal [cedecl| void $id:fun($params:params){ $items:items }|]

collectDefinitions :: C a -> C (a, [C.Definition])
collectDefinitions act = do
    oldIncludes <- gets _includes
    oldTypedefs <- gets _typedefs
    oldPrototypes <- gets _prototypes
    oldGlobals    <- gets _globals
    modify $ \s -> s { _includes = Set.empty
                     , _typedefs = []
                     , _prototypes = []
                     , _globals = []
                     }
    a  <- act
    s' <- get
    modify $ \s -> s { _includes = oldIncludes `Set.union` _includes s'
                     , _typedefs = oldTypedefs ++ _typedefs s'
                     , _prototypes = oldPrototypes ++ _prototypes s'
                     , _globals = oldGlobals ++ _globals s'
                     }
    return (a, cenvToCUnit s')
