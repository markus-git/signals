{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Core.Reify
  ( Key (..)
  , Node(..)
  , Nodes    
  , reify
  , freify
  ) where

import Signal.Core hiding (lift)
import Signal.Core.Witness

import Control.Monad.Operational.Compositional
import Control.Applicative    ((<$>))
import Control.Arrow          (first, second)
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Data.Typeable          (Typeable)
import Data.Dynamic           (Dynamic, toDyn)
import Data.Ref
import Data.Ref.Map           (Map, Name)
import Language.Embedded.VHDL (PredicateExp)
import System.Mem.StableName

import qualified Signal.Core  as S
import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right, join)

--------------------------------------------------------------------------------
-- * Reification of Signals
--------------------------------------------------------------------------------

-- | ...
data Key (i :: (* -> *) -> * -> *) (a :: *)
  where
    Key :: Name (S Symbol i a) -> Key i a

-- | ...
data Node (i :: (* -> *) -> * -> *) (a :: *)
  where
    Node :: Witness i a => S Key i a -> Node i (S Symbol i a)

-- | Reification of a signal into a mapping over its nodes and root key
reify :: Sig i a -> IO (Key i (Identity a), Nodes i)
reify (Sig (Signal sym)) = second fst <$> runStateT (reify' sym) (M.empty, M.empty)

-- | Reification of a signal function into a mapping over its nodes, root key and input key
freify
  :: ( PredicateExp (IExp i) a
     , Typeable i
     , Typeable a
     , Typeable b)
  => (Sig i a -> Sig i b)
  -> IO ( Key i (Identity b) -- root
        , Key i (Identity a) -- input
        , Nodes i)
freify f =
  do let (Sig (Signal (Symbol (Ref var _))), sig) =
           let a = Sig (S.variable (toDyn f)) in (a, f a)
     (key, nodes) <- reify sig
     return (key, Key var, nodes)

--------------------------------------------------------------------------------
-- ** ... 

-- | ...
type Nodes i = Map (Node i)

-- | ...
type Names   = Map Name

-- | Reification of a symbol tree
reify' :: forall i a.  Symbol i a -> Reify  i (Key i a)
reify' (Symbol ref@(Ref k node)) =
  do name <- lookupName ref
     case name of
       Just old@(Key k') -> return old
       Nothing  -> do
         insertName ref
         case node of
           (S.Var    dyn) -> insertNode ref (Node (S.Var    dyn))
           (S.Repeat str) -> insertNode ref (Node (S.Repeat str))
           (S.Map  f sig) ->
             do key  <- reify' sig
                insertNode ref (Node (S.Map f key))
           (S.Join l r) ->
             do lkey <- reify' l
                rkey <- reify' r
                insertNode ref (Node (S.Join lkey rkey))
           (S.Left   l) ->
             do lkey <- reify' l
                insertNode ref (Node (S.Left lkey))
           (S.Right  r) ->
             do rkey <- reify' r
                insertNode ref (Node (S.Right rkey))
           (S.Delay v sig) ->
             do key  <- reify' sig
                insertNode ref (Node (S.Delay v key))
           (S.Mux sig choices) ->
             do key  <- reify' sig
                keys <- forM choices $ \(c, s) ->
                  do s' <- reify' s
                     return (c, s')
                insertNode ref (Node (S.Mux key keys))

--------------------------------------------------------------------------------

-- | ...
type Reify i = StateT (Nodes i, Names) IO

-- | Insert a signal node under the given reference name
insertNode :: Ref (S Symbol i a) -> Node i (S Symbol i a) -> Reify i (Key i a)
insertNode ref@(Ref name _) node = modify (first $ M.insert ref node) >> return (Key name)

-- | Insert a reference name
insertName :: Ref (S Symbol i a) -> Reify i ()
insertName ref@(Ref name _) = modify $ second $ M.insert ref name

-- | Tries to find a reference name
lookupName :: Ref (S Symbol i a) -> Reify i (Maybe (Key i a))
lookupName ref@(Ref name _) = do
  node  <- gets (M.lookup name . snd)
  return $ case node of
    Nothing  -> Nothing
    Just old -> Just (Key old)

--------------------------------------------------------------------------------
