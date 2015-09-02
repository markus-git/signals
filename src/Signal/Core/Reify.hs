{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Core.Reify
  ( Key  (..)
  , Node (..)
  , Nodes
  , reify
  )where

import Control.Monad.Operational.Compositional
import Language.Embedded.VHDL (PredicateExp)

import Signal.Core (S, Signal(..), Sig(..), Symbol(..), U, Witness)
import Signal.Core.Stream (Stream, Str)
import qualified Signal.Core as S
import qualified Signal.Core.Stream as Str

import Control.Applicative ((<$>))
import Control.Arrow       (first, second)
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Data.Typeable (Typeable)
import Data.Ref
import Data.Ref.Map (Map, Name)
import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right)

import System.Mem.StableName -- !

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
    Node :: (Witness i a, Typeable a) => S Key i a -> Node i (S Symbol i a)

--------------------------------------------------------------------------------
-- ** Reify Monad

type Nodes i = Map (Node i)

type Names   = Map (Name)

type Reify i = StateT (Nodes i, Names) IO

--------------------------------------------------------------------------------
-- ** Helpers for insert/lookup

insertNode :: Ref (S Symbol i a) -> Node i (S Symbol i a) -> Reify i (Key i a)
insertNode ref@(Ref name _) node = modify (first $ M.insert ref node) >> return (Key name)

insertName :: Ref (S Symbol i a) -> Reify i ()
insertName ref@(Ref name _) = modify . second $ M.insert ref name

lookupName :: Ref (S Symbol i a) -> Reify i (Maybe (Key i a))
lookupName ref@(Ref name _) = do
  node  <- gets (M.lookup name . snd)
  return $ case node of
    Nothing  -> Nothing
    Just old -> Just (Key old)

--------------------------------------------------------------------------------
-- * Reification of Signals
--------------------------------------------------------------------------------

reify'
  :: forall i a. Typeable a
  => Symbol i a
  -> Reify  i (Key i a)
reify' (Symbol ref@(Ref _ node)) =
  do name <- lookupName ref
     case name of
       Just old -> return old
       Nothing  -> case node of
         (S.Repeat str) -> insertNode ref (Node (S.Repeat str))
         (S.Map  f sig) ->
           do key <- reify' sig
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
           do key <- reify' sig
              insertNode ref (Node (S.Delay v key))

--------------------------------------------------------------------------------
-- **

reify :: Typeable a => Sig i a -> IO (Key i (Identity a), Nodes i)
reify (Sig (Signal sym)) = second fst <$> runStateT (reify' sym) (M.empty, M.empty)

--------------------------------------------------------------------------------
