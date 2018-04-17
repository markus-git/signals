{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler.Linker where
{-
  ( Link  (..)
  , Linked(..)
  , Links    
  , linker

    -- ^ re-export of naming constructs
  , module Signal.Compiler.Linker.Names
  )
-}

import Signal.Core
import Signal.Core.Data
import Signal.Core.Reify
import Signal.Core.Witness
import Signal.Compiler.Knot
import Signal.Compiler.Sorter
import Signal.Compiler.Linker.Names
import qualified Signal.Core as C
import qualified Signal.Core.Witness as C
import qualified Signal.Compiler.Linker.Names as N

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.Hashable
import Data.Ref
import Data.Ref.Map (Map, Name)
import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right, Ordering)

import System.Mem.StableName (eqStableName, hashStableName) -- !

--------------------------------------------------------------------------------
-- * Linking.
--------------------------------------------------------------------------------
-- Once we have names for every wire (Names), we can substitute the old group
-- names. We will however need to hide their types as they vary between nodes.

-- | Nodes where recursive calls to other nodes have been replaced with names.
data Link exp pred a where
  Link :: Bundle (Core Symbol exp pred a) -> Link exp pred a

-- | Container for linked nodes and the names of their own output.
data Linked exp pred a where
  Linked ::
       Maybe (Core Link exp pred a)             -- ^ Symbol with incoming links.
    -> Maybe (Link exp pred a)                  -- ^ Outgoing link for symbol.
    -> Node exp pred (Core Symbol exp pred a)   -- ^ Node referenced for symbol.
    -> Linked exp pred (Core Symbol exp pred a)

-- | Linking monad.
type LinkM exp pred = State (Map (Linked exp pred))

--------------------------------------------------------------------------------

-- | Set the incoming link of a linked node.
setIncoming ::
     Core Link exp pred a
  -> Linked exp pred (Core Symbol exp pred a)
  -> Linked exp pred (Core Symbol exp pred a)
setIncoming i (Linked _ o n) = Linked (Just i) o n

-- | Set the outgoing link of a linked node.
setOutgoing ::
     Link exp pred a
  -> Linked exp pred (Core Symbol exp pred a)
  -> Linked exp pred (Core Symbol exp pred a)
setOutgoing o (Linked i _ n) = Linked i (Just o) n

--------------------------------------------------------------------------------

-- | Set the incoming and outgoing links for the referenced symbol, it then
--   returns the outgoing link.
visit ::
     M.Name (Core Symbol exp pred a)
  -> Core Link exp pred a
  -> Link exp pred a
  -> LinkM exp pred (Link exp pred a)
visit r i o =
  do modify $ M.adjust (setOutgoing o . setIncoming i) r
     return o

-- | Gets the link referenced by a name, if there is any.
visited ::
     M.Name (Core Symbol exp pred a)
  -> LinkM exp pred (Maybe (Link exp pred a))
visited r =
  do s <- get
     return $ case M.lookup r s of
       Just (Linked _ out _) -> out
       Nothing -> error $
         "Linker.linked: lookup failed: " ++ show (hashStableName r)
     

-- | Gets the node referenced by a name.
node ::
     M.Name (Core Symbol exp pred a)
  -> LinkM exp pred (Core Key exp pred a)
node r =
  do s <- get
     return $ case M.lookup r s of
       Just (Linked _ _ (Node n)) -> n
       Nothing -> error $
         "Linker.node: lookup failed: " ++ show (hashStableName r)

--------------------------------------------------------------------------------

link :: Tuple pred a => Key exp pred a -> LinkM exp pred (Link exp pred a)
link key@(Key r) =
  do ml <- visited r
     case ml of
       Just l  -> return l
       Nothing -> check r
  where
    check :: Tuple pred a
      => M.Name (Core Symbol exp pred a)
      -> LinkM exp pred (Link exp pred a)
    check r =
      do n <- node r
         case n of
           (C.Var d) ->
             do visit r (C.Var d) (Link $ bundle r)
           (C.Val e) ->
             do visit r (C.Val e) (Link $ bundle r)
           (C.Map f s) ->
             do o <- link s
                visit r (C.Map f o) (Link $ bundle r)
           (C.Pair a b) ->
             do oa <- link a
                ob <- link b
                visit r (C.Pair oa ob) $ case (oa, ob) of
                  (Link u, Link v) -> Link $ N.Pair u v
           (C.Fst a) ->
             do oa <- link a
                visit r (C.Fst oa) $ case oa of
                  Link (N.Pair u _) -> Link u
           (C.Snd b) ->
             do ob <- link b
                visit r (C.Snd ob) $ case ob of
                  Link (N.Pair _ v) -> Link v
           (C.Delay e s) ->
             do o <- link s
                visit r (C.Delay e o) (Link $ bundle r)

--------------------------------------------------------------------------------

-- | Linked nodes.... to get around a type mismatch.
data LinkedNode exp pred a where
  LinkedNode ::
       Core Link exp pred a
    -> LinkedNode exp pred (Core Symbol exp pred a)

-- | Short-hand for a map over core symbols with links to inputs.
type Links exp pred = Map (LinkedNode exp pred)

-- | Given a root and a set of graph nodes, a map over links to individual
--   outputs is produced.
linker :: Tuple pred a => Key exp pred a -> Nodes exp pred -> Links exp pred
linker (Key n) nodes = undefined
  where
    init :: Nodes exp pred -> Map (Linked exp pred)
    init = M.hmap repack
      where
        repack :: Node exp pred a -> Linked exp pred a
        repack node@(Node keys) = Linked Nothing Nothing node

    reduce :: Map (Linked exp pred) -> Map (LinkedNode exp pred)
    reduce = M.hmap repack
      where
        repack :: Linked exp pred a -> LinkedNode exp pred a
        repack (Linked (Just l) _ _) = LinkedNode l

--------------------------------------------------------------------------------
