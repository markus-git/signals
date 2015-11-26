{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler.Channels
{-  ( Identified(..)
  , Identifiers
  , Kind
  , Scope   (..)
  , Ident   (..)
  , Channel (..)
  , Channels(..)

  , fromLinks
  )-}
  where

import Signal.Core            (S(..), E(..), Symbol)
import Signal.Core.Witness
import Signal.Compiler.Linker

import Control.Monad.Operational.Higher
import Control.Monad
import Control.Monad.Identity (Identity)
import Control.Monad.State    (StateT)
import qualified Control.Monad.State as CMS

import Data.Maybe    (fromJust)
import Data.Typeable
import Data.Hashable
import Data.Ref
import Data.Ref.Map  (Map, Entry, Name)
import qualified Data.Ref.Map as RMap
import qualified Data.IntMap  as IMap

import Language.VHDL                          (Identifier)
import Language.Embedded.VHDL.Expression.Type (Kind)
import qualified Language.VHDL                          as VHDL
import qualified Language.Embedded.VHDL                 as HDL
import qualified Language.Embedded.VHDL.Expression.Type as HDL

import Prelude hiding (Left, Right)

--------------------------------------------------------------------------------
-- * Compiler constructs
--------------------------------------------------------------------------------

newtype Channels = Channels { runChannels :: IMap.IntMap Identifier }

lookup :: Name a -> Channels -> Maybe Identifier
lookup name (Channels m) = IMap.lookup (hash name) m

insert :: Name a -> Identifier -> Channels -> Channels
insert name ident (Channels m) = Channels (IMap.insert (hash name) ident m)

--------------------------------------------------------------------------------
-- ** ...

fromLinks
  :: forall i.
     ( HDL.SequentialCMD (IExp i) :<: i
     , HDL.ConcurrentCMD (IExp i) :<: i
     )
  => Links i
  -> Channels
fromLinks ls = undefined
  where
    cs :: Program i Channels
    cs = fromList (RMap.elems ls)

--------------------------------------------------------------------------------

type M i = StateT Channels (Program i)

fromList
  :: forall i.
     ( HDL.SequentialCMD (IExp i) :<: i
     , HDL.ConcurrentCMD (IExp i) :<: i
     )
  => [Entry (Linked i)]
  -> Program i Channels
fromList es = CMS.execStateT (mapM_ go es) (Channels IMap.empty)
  where
    go :: Entry (Linked i)  -> M i ()
    go (RMap.Entry name (Linked node o@(Link n))) =
      case node of
        Repeat c -> do
          declare n (Just c)
        Map f s -> do
          init o
        Delay d s -> do
          declare n (Just d)
          -- ??? 'opposite' ???
        Mux s cs -> do
          init o

    init :: forall a. Link i a -> M i ()
    init (Link names) = dist (witness :: Wit i a) names
      where
        dist :: Wit i x -> Names (S Symbol i x) -> M i ()
        dist (WE)     (name) = declare name Nothing
        dist (WP l r) (u, v) = dist l u >> dist r v

-- | Declares a VHDL variable for a name with, possibly, some init value
declare
  :: ( HDL.SequentialCMD (IExp i) :<: i
     , HDL.PredicateExp  (IExp i) a)
  => Named (S sym i (Identity a))
  -> Maybe (IExp i a)
  -> M i ()
declare (Named n) e = do
  i <- CMS.lift $ HDL.variableL e
  CMS.modify (insert n i)  

--------------------------------------------------------------------------------
{-
-- | Physical name representing a wire
data Identified a where
  Identified :: Integer -> Identified (S sym i a)

-- | Distributed identifiers
type Identifiers a = Distributed Identified a
-}
--------------------------------------------------------------------------------
-- ** Nodes with Identifiers as keys
{-
-- | Different kinds of scopes available
data Scope = Header | Global | Local

-- | Ident represents a wire with a set of names, kind and scope
data Ident (i :: (* -> *) -> * -> *) (a :: *) where
  Ident :: Witness i a => Identifiers (S Symbol i a) -> Kind -> Scope -> Ident i a

-- | A channel is a linked node where names have been reified as well.
data Channel (i :: (* -> *) -> * -> *) (a :: *) where
  Channel :: S Ident i a -> Ident i a -> Channel i (S Symbol i a)

-- | Short-hand for a mapping over channels
type Channels i = Map (Channel i)
-}
--------------------------------------------------------------------------------
-- ** Channel construction
{-
type Mapping = IMap.IntMap Integer

type Rm      = Reader Mapping

-- *** I make use of an ugly hack: a port variable is really a port input signal
--     This is due `declare` further down not taking a mode as input.
fromLinks :: Links i -> Channels i
fromLinks links = RMap.hmap (translate (fromList links)) links
  where
    translate :: Mapping -> Linked i a -> Channel i a
    translate m (Linked node link) =
      let out@(Ident i k s) = identify link
      in case node of
        Var d     -> Channel (Var d)                (Ident i k Header)
        Repeat c  -> Channel (Repeat c)             (out)
        Map f s   -> Channel (Map f $ identify s)   (out)
        Delay d s -> Channel (Delay d $ identify s) (Ident i HDL.Signal Global)
        Mux s cs  -> 
          let inp  = identify s
              inps = fmap (second identify) cs
          in Channel (Mux inp inps) out
      where
        identify :: forall i a. Link i a -> Ident i a
        identify (Link names) = Ident (dist (witness :: Wit i a) names) HDL.Variable Local
          where
            dist :: Wit i x -> Names (S Symbol i x) -> Identifiers (S Symbol i x)
            dist (WE) (Named name) = Identified $ fromJust $ IMap.lookup (hash name) m
            dist (WP l r)   (u, v) = (dist l u, dist r v)
-}
--------------------------------------------------------------------------------

-- | Short-hand for state used in `fromList`
--type Sm = State (Integer, Mapping)
{-
-- | Generates a mapping from name to identifier for each entry
fromList :: Links i -> Mapping
fromList ls = snd $ CMS.execState (mapM_ add $ RMap.elems ls) (0, IMap.empty)
  where
    add :: forall i. Entry (Linked i) -> Sm ()
    add (RMap.Entry _ (Linked _ (Link names :: Link i a)))
        = dist (witness :: Wit i a) names
      where
        dist :: Wit i x -> Names (S Symbol i x) -> Sm ()
        dist (WE)     (name) = next >>= insert name
        dist (WP l r) (u, v) = dist l u >> dist r v

-- | Generate a unique identifier
next :: Sm Integer
next = do
  (i, m) <- CMS.get
  CMS.put (i + 1, m)
  return i

-- | ...
insert :: Names (S Symbol i (Identity x)) -> Integer -> Sm ()
insert (Named name) i = CMS.modify (second (IMap.insert (hash name) i))
-}
{-
-- | Every delay has an `opposite` which is used in the combinatorial process
opposite :: Identifier -> Identifier
opposite (VHDL.Ident i) = VHDL.Ident $ i ++ "_in"
-}
--------------------------------------------------------------------------------

-- | Usefulness refers to whether we should generate code for the node or not
useful :: RMap.Entry (Linked i) -> Bool
useful (RMap.Entry name (Linked node link)) =
  case node of
    Var    {} -> True
    Repeat {} -> True
    Map    {} -> True
    Delay  {} -> True
    Mux    {} -> True
    _         -> False

--------------------------------------------------------------------------------
