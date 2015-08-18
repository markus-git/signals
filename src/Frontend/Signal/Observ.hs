{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Signal.Observ where

import Core (IExp)
import Frontend.Signal (S, Signal(..), Sig(..), Symbol(..), U, Witness)
import Frontend.Stream (Stream, Str)

import qualified Frontend.Signal as S
import qualified Frontend.Stream as Str

import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Data.Typeable (Typeable)
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

-- | ...
data Node (i :: (* -> *) -> * -> *) (a :: *)
  where
    Node :: (Witness a, Typeable a) => S Key i a -> Node i (S Symbol i a)

--------------------------------------------------------------------------------

-- | ...
reify :: Typeable a
  => Sig i a
  -> IO ( Key i (Identity a)
        , Map (Node i))
reify (Sig (Signal sym)) =
  do (Key k, ns, _) <- reify_node sym M.empty M.empty
     return (Key k, ns)
 
-- | ...
--
-- ! I see a pattern here...
reify_node :: forall i a. Typeable a
  => Symbol i a
  -> Map (Node i)
  -> Map (Name)
  -> IO ( Key i a
        , Map (Node i)
        , Map (Name))
reify_node (Symbol ref@(Ref name s)) nodes names
  | Just old <- M.lookup name names =
      do putStrLn $ "** reify_node: " ++ show (hashStableName name) ++ "<" ++ (print_s s) ++ "> is old"
         return (Key old, nodes, names)
  | otherwise =
      do putStrLn $ "** reify_node: " ++ show (hashStableName name) ++ "<" ++ (print_s s) ++ "> is new"
         let names' = M.insert ref name names
         case s of
           (S.Repeat (s :: Stream i (IExp i b))) ->
             do let node     = Node (S.Repeat s) :: Node i (S Symbol i a) 
                    nodes'   = M.insert ref node nodes
                return (Key name, nodes', names')

           (S.Map (f :: Stream i (U i b) -> Stream i (U i a)) (s :: Symbol i b)) ->
             do (k, nodes', names'') <- reify_node s nodes names'
                let node     = Node (S.Map f k) :: Node i (S Symbol i a)
                    nodes''  = M.insert ref node nodes'
                return (Key name, nodes'', names'')

           (S.Join (l :: Symbol i b) (r :: Symbol i c)) ->
             do (kl, nodes',  names'')  <- reify_node l nodes  names'
                (kr, nodes'', names''') <- reify_node r nodes' names''
                let node     = Node (S.Join kl kr) :: Node i (S Symbol i a)
                    nodes''' = M.insert ref node nodes''
                return (Key name, nodes''', names''')

           (S.Left (l :: Symbol i (a, b))) ->
             do (k, nodes', names'') <- reify_node l nodes names'
                let node     = Node (S.Left k) :: Node i (S Symbol i a)
                    nodes''  = M.insert ref node nodes'
                return (Key name, nodes'', names'')

           (S.Right (r :: Symbol i (b, a))) ->
             do (k, nodes', names'') <- reify_node r nodes names'
                let node     = Node (S.Right k) :: Node i (S Symbol i a)
                    nodes''  = M.insert ref node nodes'
                return (Key name, nodes'', names'')
                
           (S.Delay (e :: IExp i b) (s :: Symbol i (Identity b))) ->
             do (k, nodes', names'') <- reify_node s nodes names'
                let node     = Node (S.Delay e k) :: Node i (S Symbol i a)
                    nodes''  = M.insert ref node nodes'
                return (Key name, nodes'', names'')

--------------------------------------------------------------------------------

print_node :: forall i a. Node i a -> String
print_node (Node s) = print_s s

print_s :: forall sym i a. S sym i a -> String
print_s s = case s of
  (S.Repeat str) -> "Repeat"
  (S.Map f s)    -> "Map"
  (S.Join l r)   -> "Zip"
  (S.Left p)     -> "Left"
  (S.Right p)    -> "Right"
  (S.Delay v s)  -> "Delay"
