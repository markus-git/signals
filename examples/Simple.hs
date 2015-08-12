{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}

module Simple where

import Core
import Frontend.Signal hiding (S)
import Frontend.Signal.Observ
import Backend.Compiler.Cycles
import Backend.Compiler.Sorter
import Backend.Compiler.Linker

import Data.Functor.Identity
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

     let c = cycles k m
         s = sorter k m
         l = linker s m
         
     putStrLn $ "\n -- Printing signal tree --\n -"
     putStrLn $ " - cycles?   " ++ show c
     putStrLn $ " - ordering? " ++ show s
     putStrLn $ " - links?    " ++ pp_linked l
     putStrLn $ ""
     putStrLn $ pp_nodes k m ++ "\n"
  where
    pp_nodes :: Key i a -> Map (Node i) -> String
    pp_nodes (Key k) m 
      | Just (Node sig) <- M.lookup k (M.hmap id m) =
          case sig of
            (Repeat str) -> "Repeat"
            (Map f s)    -> "Map ("   ++ pp_nodes s m ++ ")"
            (Join l r)   -> "Zip ("   ++ pp_nodes l m ++ ", "
                                      ++ pp_nodes r m ++ ")"
            (Left p)     -> "Left ("  ++ pp_nodes p m ++ ")"
            (Right p)    -> "Right (" ++ pp_nodes p m ++ ")"
            (Delay v s)  -> "Delay (" ++ pp_nodes s m ++ ")"
      | otherwise = "Doh!"

    pp_linked :: Map (Linked i) -> String
    pp_linked m =
      let d = fmap snd $ concat $ M.dump m
      in  unwords $ fmap apa d
      where
        apa :: M.HideType (Linked i) -> String
        apa (M.Hide (Linked sym out)) = case sym of
          (Repeat c)  -> "Repeat"
          (Map f s)   -> "Map" 
          (Join l r)  -> "Zip" 
          (Left l)    -> "Left"
          (Right r)   -> "Right"
          (Delay v s) -> "Delay"

--------------------------------------------------------------------------------

instance Show (Ordered i) where
  show (Ordered n) = show (hashStableName n)

instance Show (Link i a) where
  show (Link n) = "todo"
