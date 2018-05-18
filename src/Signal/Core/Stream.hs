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

repeat :: exp a -> Str instr exp pred a
repeat a = Stream $ return $ return a

map :: (exp a -> exp b) -> Str instr exp pred a -> Str instr exp pred b
map f (Stream init) = Stream $
  do next <- init
     return $
       do val <- next
          return $ f val

zipWith :: (exp a -> exp b -> exp c) -> Str instr exp pred a -> Str instr exp pred b -> Str instr exp pred c
zipWith f (Stream initL) (Stream initR) = Stream $
  do nextL <- initL
     nextR <- initR
     return $
       do valL <- nextL
          valR <- nextR
          return $ f valL valR

--------------------------------------------------------------------------------

-- | Run stream to produce transition action
run :: Stream instr exp pred a -> Program instr (Param2 exp pred) a
run (Stream init) = join init

--------------------------------------------------------------------------------
