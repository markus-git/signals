{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Signal.Observ where

import Core (IExp)
import Frontend.Signal (S, Signal(..), Sig(..), Symbol(..), U)
import Frontend.Stream (Stream, Str)
import Backend.Nested

import qualified Frontend.Signal as S
import qualified Frontend.Stream as Str

import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Data.Ref
import Data.Ref.Map (Map, Name)

import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right)


import System.Mem.StableName -- *** temp

--------------------------------------------------------------------------------
-- * Graph representation of Signals
--------------------------------------------------------------------------------

-- | ...
data Key (i :: (* -> *) -> * -> *) (a :: *)
  where
    Key :: Name (S Symbol i a) -> Key i a
  deriving Eq

-- | nodes in our graph should now be parameterized on references instead of
-- recursive calls to another symbol
data Node (i :: (* -> *) -> * -> *) (a :: *)
  where
    Node :: S Key i a -> Node i (S Symbol i a)

--------------------------------------------------------------------------------
-- ** ...

reify :: Sig i a -> IO (Key i (Identity a), Map (Node i))
reify (Sig (Signal sym)) = do
  putStrLn ("\n* reify: start")
  (Key k, ns) <- reify_node sym M.empty
  putStrLn ("* reify: root is " ++ show (hashStableName k)) >> M.debug ns reify_go
  return (Key k, ns)

reify_node :: forall i a. Symbol i a -> Map (Node i) -> IO (Key i a, Map (Node i))
reify_node (Symbol ref@(Ref name s)) nodes
  | Just _ <- M.lookup name nodes =
      do let n = hashStableName name
         putStrLn $ "** reify_node: " ++ show n ++ " has already been added"
         return (Key name, nodes)
  | otherwise =
      do let n = hashStableName name
         putStrLn $ "** reify_node: " ++ show n ++ " is new"
         case s of
           (S.Repeat (s :: Stream i (IExp i b))) ->
             do let node    = Node (S.Repeat s) :: Node i (S Symbol i a) 
                    nodes'  = M.insert ref node nodes
                return (Key name, nodes')
           (S.Map (f :: Stream i (U i b) -> Stream i (U i a)) (s :: Symbol i b)) ->
             do (k, nodes') <- reify_node s nodes
                let n       = let (Key x) = k in hashStableName x
                    node    = Node (S.Map f k) :: Node i (S Symbol i a)
                    nodes'' = M.insert ref node nodes'
                return (Key name, nodes'')

-- *** ToDo : Your really need to add names! 
--------------------------------------------------------------------------------

reify_go :: forall i a. Node i a -> String
reify_go (Node s) = case s of
  (S.Repeat str) -> "Repeat"
  (S.Map f s)    -> "Map"
  (S.Join l r)   -> "Zip"
  (S.Left p)     -> "Left"
  (S.Right p)    -> "Right"
  (S.Delay v s)  -> "Delay"

