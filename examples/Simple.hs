{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Simple where

import Core
import Frontend.Signal hiding (S)
import Frontend.Signal.Observ
import Backend.Compiler.Cycles
import Backend.Compiler.Sorter

import Data.Ref
import Data.Ref.Map (Map, Name)
import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right, Ordering)

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
test_sig = let x = int 1 in add x (neg x)

test =
  do (k, m) <- reify test_sig
     let srt = sorter k m
         out = go k m srt
         
     putStrLn $ "\n -- Printing signal tree --\n -"
     putStrLn $ " - cycles? " ++ show (cycles k m)
     putStrLn $ ""
     putStrLn $ out ++ "\n"
  where
    go :: Key i a -> Map (Node i) -> Map (Ordering i) -> String
    go (Key k) m srt
      | Just (Node sig) <- M.lookup k (M.hmap id m) =
          let (Ordering o) = srt M.! k in
          case sig of
            (Repeat str) -> "Repeat <" ++ show o ++ ">"
            (Map f s)    -> "Map <"    ++ show o ++ "> (" ++ go s m srt ++ ")"
            (Join l r)   -> "Zip <"    ++ show o ++ "> (" ++ go l m srt ++ ", " ++ go r m srt ++ ")"
            (Left p)     -> "Left <"   ++ show o ++ "> (" ++ go p m srt ++ ")"
            (Right p)    -> "Right <"  ++ show o ++ "> (" ++ go p m srt ++ ")"
            (Delay v s)  -> "Delay <"  ++ show o ++ "> (" ++ go s m srt ++ ")"
      | otherwise = "Doh!"

--------------------------------------------------------------------------------
