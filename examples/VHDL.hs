{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples.VHDL where

import Core hiding (compile)

import Frontend.Stream (Str)
import Frontend.Signal (Sig)
import Frontend.Signal.Observ
import qualified Frontend.Stream as Str
import qualified Frontend.Signal as Sig

import Backend.Compiler
import Backend.Compiler.Cycles
import Backend.Compiler.Sorter
import Backend.Compiler.Linker

import Backend.VHDL.Expr (Expr)
import Backend.VHDL.CMD
import qualified Backend.VHDL.Expr as E

import Prelude hiding (not, and, or)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

type E    = Expr

type I    = ConcurrentCMD E

type Prog = Program I

type S    = Sig I

--------------------------------------------------------------------------------

true :: S Bool
true = Sig.lift0 $ E.bool True

false :: S Bool
false = Sig.lift0 $ E.bool False

not :: S Bool -> S Bool
not = Sig.lift1 E.not

and, or, xor, xnor, nand, nor :: S Bool -> S Bool -> S Bool
and  = Sig.lift2 E.and
or   = Sig.lift2 E.or
xor  = Sig.lift2 E.xor
xnor = Sig.lift2 E.xnor
nand = Sig.lift2 E.nand
nor  = Sig.lift2 E.nor

--------------------------------------------------------------------------------

test :: IO ()
test =
  do prg <- test_program test_signal
     putStrLn $ compile prg

test_program :: S Bool -> IO (Prog (E Bool))
test_program sig =
  do str <- compiler sig
     return $ do
       Str.run str

test_signal :: S Bool
test_signal = let t = true in and t (not t)

--------------------------------------------------------------------------------
