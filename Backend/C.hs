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

-- |
compExp' :: Expr a -> C C.Exp
compExp' (Var v)   = return [cexp| $id:v |]
compExp' (Val v)   = let s = show v in return [cexp| $id:s |]

-- ^ Math. ops.
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
compExp' (Exp a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' ^ $b' |]
compExp' (Sin a)   = do
  a' <- compExp' a
  return [cexp| sin( $a' ) |]
compExp' (Mod a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' % $b'|]

-- ^ Bool. ops.
compExp' (Not  a)  = do
  a' <- compExp' a
  return [cexp| ! $a' |]
compExp' (Eq a b)  = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' == $b' |]
compExp' (LEq a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' <= $b' |]

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

-- ^ File handling
compCMD' (Open path) = do
  addInclude "<stdio.h>"
  addInclude "<stdlib.h>"
  sym <- gensym "v"
  addLocal [cdecl| typename FILE * $id:sym; |]
  addStm   [cstm| $id:sym = fopen($id:path, "a+"); |]
  return $ Ptr sym
compCMD' (Close ptr) = do
  let ptr' = unPtr ptr
  addStm [cstm| fclose($id:ptr'); |]
compCMD' (Put ptr exp) = do
  let ptr' = unPtr ptr
  v <- compExp exp
  addStm [cstm| fprintf($id:ptr', "%f", $v); |]
compCMD' (Get ptr) = do
  let ptr' = unPtr ptr
  sym <- gensym "v"
  addLocal [cdecl| float $id:sym; |]
  addStm   [cstm| $id:sym = atof(fgetc($id:ptr')); |]
  return $ Var sym

-- ^ Mutable refrences
compCMD' (InitRef) = do
  sym <- gensym "r"
  addLocal [cdecl| float $id:sym; |] -- todo: get real type
  return $ Ref sym
compCMD' (NewRef exp) = do
  sym <- gensym "r"
  v   <- compExp exp
  addLocal [cdecl| float $id:sym; |] -- todo: get real type
  addStm   [cstm| $id:sym = $v; |]
  return $ Ref sym
compCMD' (GetRef ref) = do
  let ref' = unRef ref
  sym <- gensym "r"
  addLocal [cdecl| float $id:sym; |]
  addStm   [cstm| $id:sym = $id:ref'; |]
  return $ Var sym
compCMD' (SetRef ref exp) = do
  let ref' = unRef ref
  v <- compExp exp
  addStm [cstm| $id:ref' = $v; |]

-- ^ Mutable arrays
compCMD' (NewArr size init) = do
  addInclude "<string.h>"
  sym <- gensym "a"
  v   <- compExp size
  i   <- compExp init -- todo: use this with memset
  addLocal [cdecl| float $id:sym[ $i ]; |]
  addStm   [cstm| memset($id:sym, 0, sizeof( $id:sym )); |]
  return $ Arr sym
compCMD' (GetArr expi arr) = do
  let arr' = unArr arr
  sym <- gensym "v"
  i   <- compExp expi
  addLocal [cdecl| float $id:sym; |] -- todo: get real type
  addStm   [cstm| $id:sym = $id:arr'[ $i ]; |]
  return $ Var sym
compCMD' (SetArr expi expv arr) = do
  let arr' = unArr arr
  v <- compExp expv
  i <- compExp expi
  addStm [cstm| $id:arr'[ $i ] = $v; |]

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
