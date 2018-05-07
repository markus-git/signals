{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Core.Reify
  ( Key(..)
  , Node(..)
  , Nodes    
  , reify
  , reifyF1
  ) where

import Signal.Core (Signal (..), Symbol (..), Core (..))
import Signal.Core.Witness
import qualified Signal.Core as S

import Control.Applicative    ((<$>))
import Control.Arrow          (first, second)
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Data.Typeable          (Typeable)
import Data.Dynamic           (Dynamic, toDyn)
import Data.Ref
import Data.Ref.Map           (Map, Name)
import System.Mem.StableName
import qualified Data.Ref.Map as M

-- operational-alacarte
import Control.Monad.Operational.Higher

import Prelude hiding (Left, Right, join)

--------------------------------------------------------------------------------
-- * Reification of Signals.
--------------------------------------------------------------------------------

-- | Index to a core symbol.
data Key exp pred a
  where
    Key :: Name (Core Symbol exp pred a) -> Key exp pred a

-- | A core construct with references.
data Node exp pred a
  where
    Node :: Core Key exp pred a -> Node exp pred (Core Symbol exp pred a)

-- | Short-hand for a node mapping.
type Nodes exp pred = Map (Node exp pred)

-- | Reification monad.
type Reify exp pred = StateT (Nodes exp pred, Map Name) IO

--------------------------------------------------------------------------------

-- | Reification of a signal into a mapping over its nodes and root key.
reify :: Signal exp pred a -> IO (Key exp pred a, Nodes exp pred)
reify (Signal sym) = second fst <$> runStateT (reify' sym) (M.empty, M.empty)

-- | Reification of a 1-ary signal function.
reifyF1 ::
     ( Typeable exp
     , Typeable pred
     , Typeable a
     , Typeable b
     , pred a
     )
  => (Signal exp pred (Identity a) -> Signal exp pred b)
  -> IO (Key exp pred b, Nodes exp pred)
reifyF1 f =
  let (_, graph) = let a = S.var (toDyn f) in (a, f a)
  in reify graph

reifyF2 ::
  ( Typeable exp
  , Typeable pred
  , Typeable a
  , Typeable b
  , Typeable c
  )
  => (Signal exp pred a -> Signal exp pred b -> Signal exp pred c)
  -> IO (Key exp pred b, Nodes exp pred)
reifyF2 = undefined

--------------------------------------------------------------------------------

-- | Reification of a symbol tree.
reify' :: forall exp pred a .
     Symbol exp pred a
  -> Reify exp pred (Key exp pred a)
reify' (Symbol ref@(Ref k node)) =
  do name <- lookupName ref
     case name of
       Just old@(Key k') -> return old
       Nothing  ->
         do insertName ref
            case node of
              (S.Var dyn) -> insertNode ref (Node (S.Var dyn))
              (S.Val str) -> insertNode ref (Node (S.Val str))
              (S.Map f sig) ->
                do key  <- reify' sig
                   insertNode ref (Node (S.Map f key))
              (S.Pair l r) ->
                do lkey <- reify' l
                   rkey <- reify' r
                   insertNode ref (Node (S.Pair lkey rkey))
              (S.Fst l) ->
                do lkey <- reify' l
                   insertNode ref (Node (S.Fst lkey))
              (S.Snd r) ->
                do rkey <- reify' r
                   insertNode ref (Node (S.Snd rkey))
              (S.Delay v sig) ->
                do key  <- reify' sig
                   insertNode ref (Node (S.Delay v key))
{-
           (S.Mux sig choices) ->
             do key  <- reify' sig
                keys <- forM choices $ \(c, s) ->
                  do s' <- reify' s
                     return (c, s')
                insertNode ref (Node (S.Mux key keys))
-}
--------------------------------------------------------------------------------

-- | Insert a signal node under the given reference name.
insertNode ::
     Ref (Core Symbol exp pred a)
  -> Node exp pred (Core Symbol exp pred a)
  -> Reify exp pred (Key exp pred a)
insertNode ref@(Ref name _) node =
  do modify (first $ M.insert ref node)
     return (Key name)

-- | Insert a reference name.
insertName ::
     Ref (Core Symbol exp pred a)
  -> Reify exp pred ()
insertName ref@(Ref name _) =
  do modify $ second $ M.insert ref name

-- | Tries to find a reference name.
lookupName ::
     Ref (Core Symbol exp pred a)
  -> Reify exp pred (Maybe (Key exp pred a))
lookupName ref@(Ref name _) =
  do node  <- gets (M.lookup name . snd)
     return $ case node of
       Nothing  -> Nothing
       Just old -> Just (Key old)

--------------------------------------------------------------------------------
-- Debugging.
--------------------------------------------------------------------------------
{-
debug :: (Key i (Identity a), Nodes i) -> IO ()
debug ((Key name), nodes) =
  do let entries = M.elems nodes
     putStrLn "========== Debug =========="
     putStrLn $ "Input key: " ++ show (hashStableName name)
     forM_ entries (putStrLn . apa)
     putStrLn "==========================="
  where
    apa :: M.Entry (Node i) -> String
    apa (M.Entry n (Node s)) =
      "Node (" ++ show (hashStableName n) ++ ") : " ++ print s
    
    print :: S sym i a -> String
    print node = case node of
      Repeat {} -> "repeat"
      Map    {} -> "map"
      Join   {} -> "join"
      Left   {} -> "left"
      Right  {} -> "right"
      Delay  {} -> "delay"
      Mux    {} -> "mux"
      Var    {} -> "var"
-}
--------------------------------------------------------------------------------
