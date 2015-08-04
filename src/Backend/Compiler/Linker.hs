{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Compiler.Linker {-(
    Resolution(..)
  , TEx
  , linker
  )-}
where

import Core
import Frontend.Stream 
import Frontend.Signal
import Frontend.Signal.Observ
import Backend.Compiler.Sorter (Order, Ordered(..))

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import Data.Ref
import Data.Ref.Map (Map, Name)
import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right, Ordering)

--------------------------------------------------------------------------------
-- * Linking
--------------------------------------------------------------------------------

-- ! Naming things has always been tricky..

type family Names a
type instance Names (S sym i (Identity a)) = Named (S sym i (Identity a))
type instance Names (S sym i (a, b))       = (Names (S sym i a), Names (S sym i b))

data Named a
  where
    Named  :: Name  (S sym i a)      -> Named (S sym i a)
    Lefty  :: Named (S sym i (a, b)) -> Named (S sym i a)
    Righty :: Named (S sym i (a, b)) -> Named (S sym i b)

-- | ...
name :: forall sym i a. Witness a => Name (S sym i a) -> Names (S sym i a)
name n = go (wit :: Wit a) (Named n)
  where
    go :: Wit x -> Named (S sym i x) -> Names (S sym i x)
    go (WE)     n = n
    go (WP l r) n = (go l (Lefty n), go r (Righty n))

--------------------------------------------------------------------------------

-- ! What should 'f' be?..
--     insert :: Name a -> f a -> Map f -> Map f
--     lookup :: Name a -> Map f -> Maybe (f a)

-- These contain names for input
data Link   (i :: (* -> *) -> * -> *) (a :: *)
  where
    Link :: Names (S Symbol i a) -> Link i a

-- These contain names for output
data Linked (i :: (* -> *) -> * -> *) (a :: *)
  where
    Linked :: S Link i a -> Names (S Symbol i a) -> Linked i (S Symbol i a)

--------------------------------------------------------------------------------

type M i = ReaderT (Map (Node i)) (State (Map (Linked i)))

node :: Name (S Symbol i a) -> M i (S Key i a)
node r = asks $ (\(Node n) -> n) . (M.! r)

new  :: Name a -> Linked i a -> M i ()
new r l = modify $ M.insert (Ref r undefined) l

old  :: Key i a -> M i (Link i a)
old (Key k) = gets $ Link . (\(Linked _ n) -> n) . (M.! k)

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

linker :: [Ordered i] -> Map (Node i) -> Map (Linked i)
linker order nodes = undefined
                   . flip execState M.empty
                   . flip runReaderT nodes
                   $ forM_ order link'

--------------------------------------------------------------------------------

link' :: Ordered i -> M i ()
link' = undefined
{-
link' :: forall i a. Witness a => Name (S Symbol i a) -> M i ()
link' sym =
  do let out = name sym
     n <- node sym
     case n of
       (Repeat c) -> do
         new sym $ Linked (Repeat c) out
       (Map  f s) -> do
         ms <- old s
         new sym $ Linked (Map f ms) out
       (Join l r) -> do
         ml@(Link p) <- old l
         mr@(Link q) <- old r
         new sym $ Linked (Join ml mr) (p, q)
       (Left   l) -> do
         ml@(Link (p, _)) <- old l
         new sym $ Linked (Left ml) p
       (Right  r) -> do
         mr@(Link (_, q)) <- old r
         new sym $ Linked (Right mr) q
       (Delay v s) -> do
         ms <- old s
         new sym $ Linked (Delay v ms) out
-}
--------------------------------------------------------------------------------
