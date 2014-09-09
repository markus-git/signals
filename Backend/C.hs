{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backend.C where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Operational

import Language.C.Quote.C
import qualified Language.C.Syntax as C
import qualified Data.Set          as Set

import Text.PrettyPrint.Mainland

import Expr
import Core
import Interpretation
import Backend.C.Monad

--------------------------------------------------------------------------------
-- * Compilation of Expressions
--------------------------------------------------------------------------------

class    Any a
instance Any a

instance CompExp C Expr
  where
    type VarPred Expr = Any

    varExp  = Var
    compExp = compExp'

compExp' :: Expr a -> C C.Exp
compExp' (Var v)   = return [cexp| $id:v |]
compExp' (Add a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' + $b' |]
compExp' (Sub a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' - $b' |]
compExp' (Mul a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' * $b' |]
compExp' (Div a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' / $b' |]

--------------------------------------------------------------------------------
-- * Compilation of Commands
--------------------------------------------------------------------------------

instance CompCMD C (CMD Expr)
  where
    compCMD = compCMD'

instance CompCMD C cmd => CompCMD C (Construct cmd)
  where
    compCMD = compConstruct

compCMD' :: CMD Expr a -> C a
compCMD' (Open path) = do
  addInclude "<stdio.h>"
  addInclude "<stdlib.h>"
  sym <- gensym "v"
  addLocal [cdecl| char * $id:sym; |]
  addStm   [cstm| $id:sym = fopen($id:path, "a+"); |]
  return $ Ptr sym
compCMD' (Close ptr) = do
  let ptr' = unPtr ptr
  addInclude "<stdio.h>"
  addStm [cstm| fclose($id:ptr'); |]
compCMD' (Put ptr v) = do
  let ptr' = unPtr ptr
  addInclude "<stdio.h>"
  addInclude "<stdlib.h>"
  v' <- compExp v
  addStm [cstm| fprintf($id:ptr', "$f", $v'); |]
compCMD' (Get ptr)   = do
  let ptr' = unPtr ptr
  addInclude "<stdio.h>"
  sym <- gensym "v"
  addLocal [cdecl| float $id:sym; |]
  addStm   [cstm| $id:sym = atof(fgetc($id:ptr')); |]
  return $ Var sym

compConstruct :: CompCMD C cmd => Construct cmd a -> C a
compConstruct (Function fun body) = inFunction fun $ compile body

compile :: CompCMD C cmd => Program cmd a -> C a
compile = interpretWithMonad compCMD

--------------------------------------------------------------------------------
-- * Run Functions
--------------------------------------------------------------------------------

cgen :: CompCMD C cmd => Program cmd a -> IO Doc
cgen = cgen' Flags

cgen' :: CompCMD C cmd => Flags -> Program cmd a -> IO Doc
cgen' flags ma = do
    (_,cenv) <- runC (compile ma) (defaultCEnv flags)
    return $ ppr $ cenvToCUnit cenv
