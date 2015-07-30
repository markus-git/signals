{-# LANGUAGE TypeOperators #-}

module Backend.VHDL.Test where

import Control.Monad.Operational.Compositional

import Backend.VHDL.Expr     hiding (compile)
import Backend.VHDL.CMD
import Backend.VHDL.Syntax
import Backend.VHDL.Generate hiding (not, and, or, xor)

import Prelude hiding (not, and, or)

--------------------------------------------------------------------------------

type CMD = ConcurrentCMD E :+: HeaderCMD E

type E   = Expr
type P   = Program CMD

--------------------------------------------------------------------------------

main :: IO ()
main = putStrLn $ compile prog

prog :: P ()
prog =
  do a <- input "a"
     b <- input "b"
     c <- input "c"
     o <- output "o"
     o <=: not (xor a (xor b c))
