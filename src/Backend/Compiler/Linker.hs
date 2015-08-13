{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Compiler.Linker {-(
    Dist
  , Named(..)
  , Names
  , Resolution(..)
  , Link(..)
  , Linked(..)
  , linker
  )-}
  where

import Core
import Frontend.Stream 
import Frontend.Signal
import Frontend.Signal.Observ
import Backend.Knot
import Backend.Compiler.Sorter

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.Hashable
import Data.Ref
import Data.Ref.Map (Map, Name)
import qualified Data.Ref.Map as M

import Unsafe.Coerce

import Prelude hiding (Left, Right, Ordering)

--------------------------------------------------------------------------------
-- * 
--------------------------------------------------------------------------------

-- ! This doesn't belong here, new module?
type family Dist (p :: * -> *) a
type instance Dist p (S sym i (Identity a)) = p (S sym i (Identity a))
type instance Dist p (S sym i (a, b))       = (Dist p (S sym i a), Dist p (S sym i b))

-- Naming things has always been tricky..
data Named a
  where
    Named  :: Name  (S sym i a)      -> Named (S sym i a)
    Lefty  :: Named (S sym i (a, b)) -> Named (S sym i a)
    Righty :: Named (S sym i (a, b)) -> Named (S sym i b)

instance Hashable (Named a)
  where
    hashWithSalt s (Named n)  = s `hashWithSalt` n
    hashWithSalt s (Lefty l)  = s `hashWithSalt` (0 :: Int) `hashWithSalt` l
    hashWithSalt s (Righty r) = s `hashWithSalt` (1 :: Int) `hashWithSalt` r

-- | ...
type Names a = Dist Named a

-- | Takes a composite name and creates unique names for each part
name :: forall sym i a. Witness a => Name (S sym i a) -> Names (S sym i a)
name n = go (wit :: Wit a) (Named n)
  where
    go :: Wit x -> Named (S sym i x) -> Names (S sym i x)
    go (WE)     n = n
    go (WP l r) n = (go l (Lefty n), go r (Righty n))

--------------------------------------------------------------------------------
-- ** Once we have names for every wire, we can substitute the old group names

-- | Nodes where recursive calls to other nodes have been replaced with names
data Link   (i :: (* -> *) -> * -> *) (a :: *)
  where
    Link :: Witness a => Names (S Symbol i a) -> Link i a

-- | Container for linked nodes and the names of their own output
data Linked (i :: (* -> *) -> * -> *) (a :: *)
  where
    Linked :: S Link i a -> Link i a -> Linked i (S Symbol i a)

--------------------------------------------------------------------------------
-- ** We will however need to hide their types as they vary between nodes

data Hide f where
  Hide :: f a -> Hide f

data Pair f g a where
  Pair :: f a -> g a -> Pair f g a

type Item i = Hide (Pair Name (Linked i))

--------------------------------------------------------------------------------
-- * Linking monad
--------------------------------------------------------------------------------

type Resolution i = Map (Linked i)

type Constraint i = Item i

type M i          = Knot (Resolution i) (Constraint i) (State (Map (Node i)))

--------------------------------------------------------------------------------

node :: Name (S Symbol i a) -> M i (Node i (S Symbol i a))
node name =
  do out <- get
     return $ case M.lookup name out of
       Nothing   -> error "Linker.node:lookup failed"
       Just node -> node

resolve :: Key i a -> M i (Link i a)
resolve (Key name) =
  do out <- ask
     return $ case M.lookup name out of
       Nothing           -> error "Linker.resolve:lookup failed"
       Just (Linked _ l) -> l

output :: Item i -> M i ()
output i = tell [i]

--------------------------------------------------------------------------------
-- * Linker
--------------------------------------------------------------------------------

link' :: Ordered i -> M i ()
link' (Ordered sym) =
  do (Node n) <- node sym
     case n of
       (Repeat c) ->
         do constrain (Repeat c) (name sym)
       (Map f s) ->
         do inp <- resolve s
            constrain (Map f inp) (name sym)
       (Join l r) ->
         do inp_l <- resolve l
            inp_r <- resolve r
            constrain (Join inp_l inp_r) (reify inp_l, reify inp_r)
  where
    reify ~(Link n) = n
    constrain n l   = output $ Hide $ Pair sym $ Linked n $ Link l

--------------------------------------------------------------------------------

linker :: [Ordered i] -> Map (Node i) -> Resolution i
linker order nodes = snd . flip evalState nodes . tie solve $ forM_ order link'
  where
    solve :: Solver (Resolution i) (Constraint i)
    solve = foldr ins M.empty -- ~= fromList
      
    ins (Hide (Pair n l)) = M.insert (ref n) l        -- <--- This doesn't work
    ref = flip Data.Ref.Ref undefined

--------------------------------------------------------------------------------
