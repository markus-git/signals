{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Backend.C where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Operational
import Data.Typeable (TypeRep, typeOf)

import Language.C.Quote.C
import qualified Language.C.Syntax as C
import qualified Data.Set          as Set

import Text.PrettyPrint.Mainland

import Expr
import Core
import Interpretation
import Backend.C.Monad

--------------------------------------------------------------------------------
-- * Evaluation
--------------------------------------------------------------------------------

instance EvalExp Expr
  where
    type LitPred Expr = Show

    litExp  = Val
    evalExp = evalExpr'

-- |
evalExpr' :: Expr a -> a
evalExpr' (Val a) = a
evalExpr' (Var _) = error "cannot eval var"

-- ^ Math. ops.
evalExpr' (Add a b) = evalExpr' a + evalExpr' b
evalExpr' (Sub a b) = evalExpr' a - evalExpr' b
evalExpr' (Mul a b) = evalExpr' a * evalExpr' b
evalExpr' (Div a b) = evalExpr' a / evalExpr' b
evalExpr' (Mod a b) = evalExpr' a `mod` evalExpr' b
evalExpr' (Sin a)   = sin $ evalExpr' a
evalExpr' (I2N a)   = fromInteger $ fromIntegral $ evalExpr' a

-- ^ Bool. ops.
evalExpr' (Not   a) = not $ evalExpr' a
evalExpr' (And a b) = evalExpr' a && evalExpr' b
evalExpr' (Or  a b) = evalExpr' a || evalExpr' b
evalExpr' (Eq  a b) = evalExpr' a == evalExpr' b
evalExpr' (LEq a b) = evalExpr' a <= evalExpr' b

--------------------------------------------------------------------------------
-- * Compilation of Expressions
--------------------------------------------------------------------------------

class    Any a
instance Any a

instance CompExp C Expr
  where
    type VarPred Expr = Any
    varExp   = Var
    compExp  = compExp'

-- |
compExp' :: Expr a -> C C.Exp
compExp' (Var v) = return [cexp| $id:v |]
compExp' (Val v) = case show v of
    "True"  -> addInclude "<stdbool.h>" >> return [cexp| true |]
    "False" -> addInclude "<stdbool.h>" >> return [cexp| false |]
    v'      -> return [cexp| $id:v' |]

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
compExp' (I2N a) = do
  a' <- compExp' a
  return [cexp| $a' |]

-- ^ Bool. ops.
compExp' (Not  a)  = do
  a' <- compExp' a
  return [cexp| ! $a' |]
compExp' (And a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| ($a' && $b') |]
compExp' (Or a b)  = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| ($a' || $b') |]
compExp' (Eq a b)  = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' == $b' |]
compExp' (LEq a b) = do
  a' <- compExp' a
  b' <- compExp' b
  return [cexp| $a' <= $b' |]

compSExp' s = todo

--------------------------------------------------------------------------------
-- * Compilation of Commands
--------------------------------------------------------------------------------

compTypeRep :: TypeRep -> C.Type
compTypeRep trep = case show trep of
    "Bool"  -> [cty| int   |]
    "Int"   -> [cty| int   |]  -- todo: should only use fix-width Haskell ints
    "Float" -> [cty| float |]

instance CompCMD C (CMD Expr)
  where
    compCMD = compCMD'

instance CompCMD C cmd => CompCMD C (Construct cmd)
  where
    compCMD = compConstruct

compCMD' :: CMD Expr a -> C a

--------------------------------------------------------------------------------
-- ^ File handling
compCMD' (Open path) = do
  addInclude "<stdio.h>"
  addInclude "<stdlib.h>"
  sym <- gensym "v"
  addLocal [cdecl| typename FILE * $id:sym; |]
  addStm   [cstm| $id:sym = fopen($id:path', "r+"); |]
  return $ HandleComp sym
  where
    path' = show path
compCMD' (Close (HandleComp h)) = do
  addStm [cstm| fclose($id:h); |]
compCMD' (Put (HandleComp h) exp) = do
  v <- compExp exp
  addStm [cstm| fprintf($id:h, "%f ", $v); |]
compCMD' (Get (HandleComp h)) = do
  sym <- gensym "v"
  addLocal [cdecl| float $id:sym; |]
  addStm   [cstm| fscanf($id:h, "%f", &$id:sym); |]
  return $ Var sym
compCMD' (Eof (HandleComp h)) = do
  addInclude "<stdbool.h>"
  sym <- gensym "v"
  addLocal [cdecl| int $id:sym; |]
  addStm   [cstm| $id:sym = feof($id:h); |]
  return $ Var sym

--------------------------------------------------------------------------------
-- ^ Mutable refrences
compCMD' (NewRef exp) = do
  let t = compTypeRep undefined
  sym <- gensym "r"
  v   <- compExp exp
  addLocal [cdecl| float $id:sym; |]
  addStm   [cstm| $id:sym = $v; |]
  return $ RefComp sym
compCMD' (GetRef (RefComp ref)) = do
  let t = compTypeRep undefined
  sym <- gensym "r"
  addLocal [cdecl| float $id:sym; |]
  addStm   [cstm| $id:sym = $id:ref; |]
  return $ Var sym
compCMD' (SetRef (RefComp ref) exp) = do
  v <- compExp exp
  addStm [cstm| $id:ref = $v; |]

--------------------------------------------------------------------------------
-- ^ Mutable arrays
compCMD' (NewArr size init) = do
  addInclude "<string.h>"
  sym <- gensym "a"
  v   <- compExp size
  i   <- compExp init -- todo: use this with memset
  addLocal [cdecl| float $id:sym[ $v ]; |] -- todo: get real type
  addStm   [cstm| memset($id:sym, $i, sizeof( $id:sym )); |]
  return $ ArrComp sym
compCMD' (GetArr expi (ArrComp arr)) = do
  sym <- gensym "a"
  i   <- compExp expi
  addLocal [cdecl| float $id:sym; |] -- todo: get real type
  addStm   [cstm| $id:sym = $id:arr[ $i ]; |]
  return $ Var sym
compCMD' (SetArr expi expv (ArrComp arr)) = do
  v <- compExp expv
  i <- compExp expi
  addStm [cstm| $id:arr[ $i ] = $v; |]
compCMD' (UnsafeGetRef (RefComp ref)) =
  return $ Var ref

--------------------------------------------------------------------------------
-- ^ Control structures
compCMD' (If b t f) = do
  b' <- compExp b :: C C.Exp

  ct <- inNewBlock_ $ compile t
  cf <- inNewBlock_ $ compile f

  case null cf of
    True  -> addStm [cstm| if ($(b')) {$items:ct} |]
    False -> addStm [cstm| if ($(b')) {$items:ct} else {$items:cf} |]
  return ()
compCMD' (While b t) = do
  b'  <- compile b  :: C (Expr Bool)
  b'' <- compExp b' :: C C.Exp

  ct <- inNewBlock_ $ compile t

  addStm [cstm| while ($(b'')) {$items:ct} |]
  return ()
    -- todo: the b program should be re-executed at the end of each iteration
compCMD' Break = addStm [cstm| break; |]
compCMD' GetTime = do
    addInclude "<sys/time.h>"
    addInclude "<sys/resource.h>"
    addGlobal getTimeDef
    sym <- gensym "t"
    addLocal [cdecl| double $id:sym; |]
    addStm   [cstm| $id:sym = get_time(); |]
    return $ Var sym
compCMD' (Printf format a) = do
    addInclude "<stdio.h>"
    let format' = show format
    a' <- compExp a
    addStm [cstm| printf($id:format', $exp:a'); |]

compConstruct :: CompCMD C cmd => Construct cmd a -> C a
compConstruct (Function fun body) = inFunction fun $ compile body

compile :: CompCMD C cmd => Program cmd a -> C a
compile = interpretWithMonad compCMD

--------------------------------------------------------------------------------
-- * Helpers
--------------------------------------------------------------------------------

getTimeDef :: C.Definition
getTimeDef = [cedecl|
// From http://stackoverflow.com/questions/2349776/how-can-i-benchmark-c-code-easily
double get_time()
{
    struct timeval t;
    struct timezone tzp;
    gettimeofday(&t, &tzp);
    return t.tv_sec + t.tv_usec*1e-6;
}
|]

--------------------------------------------------------------------------------
-- * Run Functions
--------------------------------------------------------------------------------

cgen :: CompCMD C cmd => Program cmd a -> IO Doc
cgen = cgen' Flags

cgen' :: CompCMD C cmd => Flags -> Program cmd a -> IO Doc
cgen' flags ma = do
    (_,cenv) <- runC (compile ma) (defaultCEnv flags)
    return $ ppr $ cenvToCUnit cenv

genMain :: CompCMD C cmd => Program cmd a -> IO Doc
genMain prog = do
    (_,cenv) <- runC main (defaultCEnv Flags)
    return $ ppr $ cenvToCUnit cenv
  where
    main = do
      (params,items) <- inNewFunction $ compile prog >> addStm [cstm| return 0; |]
      addGlobal [cedecl| int main($params:params){ $items:items }|]
