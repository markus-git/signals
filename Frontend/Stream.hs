{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}

module Frontend.Stream where

import Core
import Control.Applicative
import Control.Monad
import Prelude (($))

--------------------------------------------------------------------------------
-- * Streams
--------------------------------------------------------------------------------

-- | Imperative model of co-iterative streams
data Stream (instr :: (* -> *) -> * -> *) (a :: *)
  where
    Stream :: Program instr (Program instr a) -> Stream instr a
    
-- | `Shorthand` for streams which produce values of type `exp a`
type Str instr a = Stream instr (IExp instr a)

--------------------------------------------------------------------------------

-- | ...
repeat :: (e ~ IExp instr) => e a -> Str instr a
repeat a = Stream $ return $ return a

-- | ...
map :: (e ~ IExp instr) => (e a -> e b) -> Str instr a -> Str instr b
map f (Stream init) = Stream $ do
  next <- init
  return $ do
    value <- next
    return $ f value

--------------------------------------------------------------------------------

-- | Run stream to produce transition action
run :: Stream instr a -> Program instr a
run (Stream init) = join init
