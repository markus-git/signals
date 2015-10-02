{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}

module Signal.Compiler.Linker.Channels where

import Signal.Core            hiding (lift)
import Signal.Core.Reify      (Key(..))
import Signal.Core.Witness
import Signal.Compiler.Linker
import Signal.Compiler.Linker.Names

import Control.Arrow          (first, second)
import Control.Monad.Reader   (ReaderT, asks)
import Control.Monad.State    (State, modify, gets, lift)
import Control.Monad.Identity
import Control.Monad.Operational.Compositional
import qualified Control.Monad.Reader as CMR
import qualified Control.Monad.State  as CMS

import Data.Typeable
import Data.Hashable
import Data.Ref.Map           (Name)
import qualified Data.Ref.Map as Rim
import qualified Data.IntMap  as Map

import Language.VHDL          (Identifier(..))
import Language.Embedded.VHDL hiding (name)
import qualified Language.Embedded.VHDL as E

import System.Mem.StableName (eqStableName)

import Prelude hiding (Left, Right)

--------------------------------------------------------------------------------
-- * Channels
--------------------------------------------------------------------------------

data Channels = Channels {
    chan_nodes  :: Map.IntMap (Identifier, Kind)
  , chan_delays :: Map.IntMap (Identifier, Identifier) -- delays always have kind 'M.Signal'
  }

emptyChannels :: Channels
emptyChannels = Channels (Map.empty) (Map.empty)

--------------------------------------------------------------------------------
-- ** Lookup / Insert

lookupNode  :: Named (S Symbol i (Identity a)) -> Channels -> (Identifier, Kind)
lookupNode n c = (Map.!) (chan_nodes c) (hash n)

lookupDelay :: Named (S Symbol i (Identity a)) -> Channels -> (Identifier, Identifier)
lookupDelay n c = (Map.!) (chan_delays c) (hash n)

insertNode  :: Named (S Symbol i (Identity a)) -> Identifier -> Kind -> Channels -> Channels
insertNode n i k (Channels ns ds) = Channels (Map.insert (hash n) (i, k) ns) ds

insertDelay :: Named (S Symbol i (Identity a)) -> Identifier -> Identifier -> Channels -> Channels
insertDelay n ni no (Channels ns ds) = Channels ns (Map.insert (hash n) (ni, no) ds)

markNode    :: Named (S Symbol i (Identity a)) -> Kind -> Channels -> Channels
markNode n k (Channels ns ds) = Channels (Map.adjust (second (const k)) (hash n) ns) ds

--------------------------------------------------------------------------------
-- **

fromLinks :: forall i a. CompileExp (IExp i) => Key i a -> Links i -> Channels
fromLinks (Key root) links =
  let m = fmap snd $ concat $ Rim.dump links
   in CMS.evalState (foldM add emptyChannels m) 0
  where
    add :: Channels -> Rim.HideType (Linked i) -> State Int Channels
    add c (Rim.Hide (Linked (Delay {}) l@(Link out))) = newDelay l out c
    add c (Rim.Hide (Linked node       l@(Link out))) = 
      case isUseful node of
        Nothing -> return c
        Just k  -> new l out k c

    new :: forall x. Witness i x => Link i x -> Names (S Symbol i x) -> Kind -> Channels -> State Int Channels
    new _ n k c = go (witness :: Wit i x) n c
      where
        go :: Wit i y -> Names (S Symbol i y) -> Channels -> State Int Channels
        go (WP u v)   (l, r)    c = go u l c >> go v r c
        go (WE)  name@(Named n) c = do
          let kind = if root `eqStableName` n then E.Signal else k
          i <- next
          return $ insertNode name i kind c          

    newDelay :: forall x. Witness i x => Link i x -> Names (S Symbol i x) -> Channels -> State Int Channels
    newDelay _ n c = go (witness :: Wit i x) n c
      where
        go :: Wit i y -> Names (S Symbol i y) -> Channels -> State Int Channels
        go (WP u v) (l, r) c = go u l c >> go v r c
        go (WE)     (name) c = do
          old@(Ident d) <- next
          return $ insertDelay name old (Ident $ d ++ "_in") (insertNode name old E.Signal c)

    next :: State Int Identifier
    next = do i <- CMS.get
              CMS.put (i + 1)
              return (Ident $ 'v' : show i) -- ! tmp fix, replace

isUseful :: S sym i a -> Maybe Kind
isUseful (Join _ _)  = Nothing
isUseful (Left _)    = Nothing
isUseful (Right _)   = Nothing
isUseful (Var _)     = Just E.Signal
isUseful _           = Just E.Variable

--------------------------------------------------------------------------------
