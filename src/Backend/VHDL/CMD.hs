{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE Rank2Types     #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backend.VHDL.CMD where

import Control.Monad.Operational.Compositional

import Backend.VHDL.Expr
import Backend.VHDL.Syntax
import Backend.VHDL.Generate
import Backend.VHDL.Pretty

--------------------------------------------------------------------------------
--- *
--------------------------------------------------------------------------------

data ConcurrentCMD exp (prog :: * -> *) a
  where
    Assign :: Identifier -> exp a -> ConcurrentCMD exp prog ()
    Local  :: Identifier -> Type  -> ConcurrentCMD exp prog ()

instance MapInstr (ConcurrentCMD exp)
  where
    imap _ (Assign r a) = Assign r a
    imap _ (Local  i t) = Local  i t

type instance IExp (ConcurrentCMD e)       = e
type instance IExp (ConcurrentCMD e :+: i) = e

--------------------------------------------------------------------------------
-- **

(<=:) :: (ConcurrentCMD (IExp instr) :<: instr) => Identifier -> IExp instr Bool -> ProgramT instr m ()
(<=:) i = singleE . Assign i

local :: (ConcurrentCMD (IExp instr) :<: instr) => Identifier -> Type -> ProgramT instr m ()
local i = singleE . Local i

--------------------------------------------------------------------------------
-- **

compConcurrentCMD :: forall exp prog a. CompileExpr exp => ConcurrentCMD exp prog a -> LLVM a
compConcurrentCMD (Assign i exp) =
  do v <-  compExpr exp
     i <== v
compConcurrentCMD (Local i typ) =
  do localSignal i typ

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

data HeaderCMD exp (prog :: * -> *) a
  where
    Input  :: String -> HeaderCMD exp prog (exp Bool)
    Output :: String -> HeaderCMD exp prog (Identifier)

instance MapInstr (HeaderCMD exp)
  where
    imap _ (Input  str) = Input  str
    imap _ (Output str) = Output str

type instance IExp (HeaderCMD e)       = e
type instance IExp (HeaderCMD e :+: i) = e

--------------------------------------------------------------------------------
-- **

input :: (HeaderCMD (IExp instr) :<: instr) => String -> ProgramT instr m (IExp instr Bool)
input = singleE . Input

output :: (HeaderCMD (IExp instr) :<: instr) => String -> ProgramT instr m (Identifier)
output = singleE . Output

--------------------------------------------------------------------------------
-- **

compHeaderCMD :: forall exp prog a. CompileExpr exp => HeaderCMD exp prog a -> LLVM a
compHeaderCMD (Input  str) = varExpr <$> signal str In std_logic Nothing
compHeaderCMD (Output str) = signal str Out std_logic Nothing

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

instance CompileExpr exp => Interp (ConcurrentCMD exp) LLVM
  where
    interp = compConcurrentCMD

instance CompileExpr exp => Interp (HeaderCMD exp) LLVM
  where
    interp = compHeaderCMD

--------------------------------------------------------------------------------
-- **

compile :: (Interp instr LLVM, MapInstr instr) => Program instr a -> String
compile prog =
  let (h, b) = runLLVM (behavioural "test") $ interpret prog
   in unlines [show $ pp h, show $ pp b]
