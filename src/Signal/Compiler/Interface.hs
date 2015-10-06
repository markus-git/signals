module Signal.Compiler.Interface where

import Language.Embedded.VHDL.Interface

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- | General interface for compiling
class CompileExp exp => Compile exp
  where
    literal :: PredicateExp exp a => a -> exp a

--------------------------------------------------------------------------------
