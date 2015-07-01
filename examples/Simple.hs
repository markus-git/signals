{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Simple where

import Core
import Frontend.Signal hiding (S)
import Frontend.Signal.Observ
import Backend.Compiler.Cycles

import Data.Ref
import Data.Ref.Map (Map, Name)
import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right)

import System.Mem.StableName -- *** temp

--------------------------------------------------------------------------------
-- * Exp
--------------------------------------------------------------------------------

data Exp a
  where
    Val :: Int -> Exp a
    Var :: Show a => a -> Exp a

    Neg :: Exp a -> Exp a
    Add :: Exp a -> Exp a -> Exp a

type Instr = RefCMD Exp

type S     = Sig Instr

--------------------------------------------------------------------------------
-- **

int :: Int -> S Int
int i = lift0 $ Val i

neg :: S Int -> S Int
neg = lift1 Neg

add :: S Int -> S Int -> S Int
add = lift2 Add

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

test_sig :: S Int
test_sig = neg $ int 1

test =
  do (k, m) <- reify test_sig
     let out = go k m
     putStrLn $ "\n -- Printing signal tree --\n -"
     putStrLn $ " - cycles? " ++ show (cycles k m)
     putStrLn $ ""
     putStrLn $ out ++ "\n"
  where
    go :: Key i a -> Map (Node i) -> String
    go (Key k) m
      | Just (Node sig) <- M.lookup k (M.hmap id m) = case sig of
          (Repeat str) -> "Repeat"
          (Map f s)    -> "Map (" ++ go s m ++ ")"
          (Join l r)   -> "Zip (" ++ go l m ++ ", " ++ go r m ++ ")"
          (Left p)     -> "Left (" ++ go p m ++ ")"
          (Right p)    -> "Right (" ++ go p m ++ ")"
          (Delay v s)  -> "Delay (" ++ go s m ++ ")"
      | otherwise = "Doh!"

--------------------------------------------------------------------------------
