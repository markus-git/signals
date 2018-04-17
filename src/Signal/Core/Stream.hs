{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Signal.Core.Stream where

import Control.Monad.Operational.Higher

import Control.Monad (join)

--------------------------------------------------------------------------------
-- * Streams
--------------------------------------------------------------------------------

-- | Imperative model of co-iterative streams
data Stream instr exp pred a
  where
    Stream :: Program instr (Param2 exp pred) (Program instr (Param2 exp pred) a)
           -> Stream instr exp pred a
    
-- | Short-hand for streams that produce values of type `exp a`
type Str instr exp pred a = Stream instr exp pred (exp a)

--------------------------------------------------------------------------------

-- | Run stream to produce transition action
run :: Stream instr exp pred a -> Program instr (Param2 exp pred) a
run (Stream init) = join init

--------------------------------------------------------------------------------
