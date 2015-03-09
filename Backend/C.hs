{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Backend.C where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Exception
import Control.Monad.Operational
import Data.Typeable (TypeRep, typeOf)
import Data.IORef
import Data.Array.IO.Safe
import qualified System.IO as IO
import qualified Text.Printf as Printf

import Language.C.Quote.C
import qualified Language.C.Syntax as C
import qualified Data.Set          as Set

import Text.PrettyPrint.Mainland

import Core
import Interpretation

import Backend.C.Monad
import Examples.Simple.Expr

--------------------------------------------------------------------------------
-- * Compilation of Commands
--------------------------------------------------------------------------------

compile :: CompCMD cmd => Program cmd a -> C a
compile = interpretWithMonad compCMD

--------------------------------------------------------------------------------
{-
-- We cannot get this general compiler to work until we embed Typeable and
-- VarPred into some CMD constructs, which ruins a lot..
-- So this is a TODO for now at least

instance (CompExp exp, VarPred exp Int, ...) => CompCMD (CMD exp)
  where
    compCMD = compCMD'

compCMD' :: (CompExp exp, VarPred exp Int, ...) => CMD exp a -> C a
-}


instance CompCMD (CMD Expr)
  where
    compCMD = compCMD'

compCMD' :: CMD Expr a -> C a

-- ^ File handling
compCMD' (Open path) = do
  addInclude "<stdio.h>"
  addInclude "<stdlib.h>"
  sym <- gensym "v"
  addLocal [cdecl| typename FILE * $id:sym; |]
  addStm   [cstm| $id:sym = fopen($id:path', "r+"); |]
  return $ HandleComp sym
  where path' = show path
compCMD' (Close (HandleComp h)) = do
  addStm [cstm| fclose($id:h); |]
compCMD' (Put (HandleComp h) exp) = do
  v <- compExp exp
  addStm [cstm| fprintf($id:h, "%f ", $v); |]
compCMD' (Get (HandleComp h)) = do
  sym <- gensym "v"
  addLocal [cdecl| float $id:sym; |]
  addStm   [cstm| fscanf($id:h, "%f", &$id:sym); |]
  return $ varExp sym
compCMD' (Eof (HandleComp h)) = do
  addInclude "<stdbool.h>"
  sym <- gensym "v"
  addLocal [cdecl| int $id:sym; |]
  addStm   [cstm| $id:sym = feof($id:h); |]
  return $ varExp sym

-- ^ Mutable refrences
compCMD' (InitRef) = do
  let t = compTypeRep undefined -- todo
  sym <- gensym "r"
  addLocal [cdecl| float $id:sym; |]
  return $ RefComp sym
compCMD' (NewRef exp) = do
  let t = compTypeRep undefined -- todo
  sym <- gensym "r"
  v   <- compExp exp
  addLocal [cdecl| float $id:sym; |]
  addStm   [cstm| $id:sym = $v; |]
  return $ RefComp sym
compCMD' (GetRef (RefComp ref)) = do
--let t = compTypeRep $ typeOf (undefined :: a)
  sym <- gensym "r"
--addLocal [cdecl| $ty:t $id:sym; |]
  addLocal [cdecl| float $id:sym; |]
  addStm   [cstm| $id:sym = $id:ref; |]
  return $ varExp sym
compCMD' (SetRef (RefComp ref) exp) = do
  v <- compExp exp
  addStm [cstm| $id:ref = $v; |]

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
  return $ varExp sym
compCMD' (SetArr expi expv (ArrComp arr)) = do
  v <- compExp expv
  i <- compExp expi
  addStm [cstm| $id:arr[ $i ] = $v; |]

-- ^ Unsafe
compCMD' (UnsafeGetRef (RefComp ref)) =
  return $ varExp ref

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
    return $ varExp sym
compCMD' (Printf format a) = do
    addInclude "<stdio.h>"
    let format' = show format
    a' <- compExp a
    addStm [cstm| printf($id:format', $exp:a'); |]

--------------------------------------------------------------------------------
-- ** Helpers

compTypeRep :: TypeRep -> C.Type
compTypeRep trep = case show trep of
    "Bool"  -> [cty| int   |]
    "Int"   -> [cty| int   |]  -- todo: should only use fix-width Haskell ints
    "Float" -> [cty| float |]
    
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
-- * Compilation of constructs
--------------------------------------------------------------------------------

instance CompCMD cmd => CompCMD (Construct cmd)
  where
    compCMD = compConstruct
    
compConstruct :: CompCMD cmd => Construct cmd a -> C a
compConstruct (Function fun body) = inFunction fun $ compile body

--------------------------------------------------------------------------------
-- ** Run Functions

cgen :: CompCMD cmd => Program cmd a -> IO Doc
cgen = cgen' Flags

cgen' :: CompCMD cmd => Flags -> Program cmd a -> IO Doc
cgen' flags ma = do
    (_,cenv) <- runC (compile ma) (defaultCEnv flags)
    return $ ppr $ cenvToCUnit cenv

--------------------------------------------------------------------------------
-- * Evaluation of programs
--------------------------------------------------------------------------------

runProgram :: ( EvalExp exp
              , LitPred exp Bool
              , LitPred exp Float)
           => Program (CMD exp) a
           -> IO a
runProgram = interpretWithMonad runCMD

--------------------------------------------------------------------------------

readWord :: IO.Handle -> IO String
readWord h = do
    eof <- IO.hIsEOF h
    if eof
      then return ""
      else do
          c  <- IO.hGetChar h
          cs <- readWord h
          return (c:cs)

--------------------------------------------------------------------------------

runCMD :: (EvalExp exp, LitPred exp Bool, LitPred exp Float) => CMD exp a -> IO a
runCMD (Open path)            = fmap HandleEval $ IO.openFile path IO.ReadWriteMode
runCMD (Close (HandleEval h)) = IO.hClose h
runCMD (Put (HandleEval h) a) = IO.hPrint h (evalExp a)
runCMD (Get (HandleEval h))   = do
    w <- readWord h
    case reads w of
        [(f,"")] -> return $ litExp f
        _        -> error "runCMD: Get: no parse"
runCMD (Eof (HandleEval h)) = fmap litExp $ IO.hIsEOF h

runCMD (InitRef)              = fmap RefEval $ newIORef undefined -- ...
runCMD (NewRef a)             = fmap RefEval $ newIORef a
runCMD (GetRef (RefEval r))   = readIORef r
runCMD (SetRef (RefEval r) a) = writeIORef r a

runCMD (NewArr i a)               = fmap ArrEval $ newArray (0, fromIntegral (evalExp i) - 1) a
runCMD (GetArr i (ArrEval arr))   = readArray arr (fromIntegral (evalExp i))
runCMD (SetArr i a (ArrEval arr)) = writeArray arr (fromIntegral (evalExp i)) a

runCMD (UnsafeGetRef (RefEval r)) = readIORef r

runCMD (If c t f)
    | evalExp c = runProgram t
    | otherwise = runProgram f
runCMD (While cond body) = do
    cond' <- runProgram cond
    if evalExp cond'
      then runProgram body >> runCMD (While cond body)
      else return ()
runCMD Break = error "runCMD: Break not implemented"

runCMD (Printf format a) = Printf.printf format (show $ evalExp a)
