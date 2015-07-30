{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE Rank2Types     #-}

{-# LANGUAGE FlexibleContexts #-}

module Backend.VHDL.CMD where

import Control.Monad.Operational.Compositional

import Backend.VHDL.Expr
import Backend.VHDL.Syntax
import Backend.VHDL.Generate

--------------------------------------------------------------------------------
--- *
--------------------------------------------------------------------------------

data ConcurrentCMD exp (prog :: * -> *) a
  where
    Assign :: Identifier -> exp a -> ConcurrentCMD exp prog ()

instance MapInstr (ConcurrentCMD exp)
  where
    imap _ (Assign r a) = Assign r a

type instance IExp (ConcurrentCMD e)       = e
type instance IExp (ConcurrentCMD e :+: i) = e

--------------------------------------------------------------------------------
-- **

(<=:) :: (ConcurrentCMD (IExp instr) :<: instr) => Identifier -> IExp instr Bool -> ProgramT instr m ()
(<=:) i = singleE . Assign i

--------------------------------------------------------------------------------
-- **

compConcurrentCMD :: forall exp prog a. CompileExpr exp => ConcurrentCMD exp prog a -> LLVM a
compConcurrentCMD (Assign i exp) =
  do v <-  compExpr exp
     i <== v

--------------------------------------------------------------------------------
