{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Compiler.Linker (
    Dist
  , Named(..)
  , Names
  , Link(..)
  , Linked(..)
  , linker
  )
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
-- * 
--------------------------------------------------------------------------------

-- ! This doesn't belong here, new module?
type family Dist (p :: * -> *) a
type instance Dist p (S sym i (Identity a)) = p (S sym i (Identity a))
type instance Dist p (S sym i (a, b))       = ( Dist p (S sym i a)
                                              , Dist p (S sym i b))

-- Naming things has always been tricky..
data Named a
  where
    Named  :: Name  (S sym i a)      -> Named (S sym i a)
    Lefty  :: Named (S sym i (a, b)) -> Named (S sym i a)
    Righty :: Named (S sym i (a, b)) -> Named (S sym i b)

type Names a = Dist Named a

-- | Takes a composite name and creates unique names for each part
name :: forall sym i a. Witness a => Name (S sym i a) -> Names (S sym i a)
name n = go (wit :: Wit a) (Named n)
  where
    go :: Wit x -> Named (S sym i x) -> Names (S sym i x)
    go (WE)     n = n
    go (WP l r) n = (go l (Lefty n), go r (Righty n))

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

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
-- * Linker
--------------------------------------------------------------------------------

-- | ...
linker :: [Ordered i] -> Map (Node i) -> Map (Linked i)
linker order nodes = flip execState M.empty
                   . flip runReaderT nodes
                   $ forM_ order link'

--------------------------------------------------------------------------------
-- **

-- | ...
link' :: Ordered i -> M i ()
link' (Ordered sym) =
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

--------------------------------------------------------------------------------
