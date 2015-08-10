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
-- *
--------------------------------------------------------------------------------

-- | ...
data Link   (i :: (* -> *) -> * -> *) (a :: *)
  where
    Link :: Names (S Symbol i a) -> Link i a

-- | ...
data Linked (i :: (* -> *) -> * -> *) (a :: *)
  where
    Linked :: S Link i a -> Link i a -> Linked i (S Symbol i a)

--------------------------------------------------------------------------------
-- **

data Hide f where
  Hide :: f a -> Hide f

data Pair f g a where
  Pair :: f a -> g a -> Pair f g a

unsafeReveal :: Hide f -> f a
unsafeReveal (Hide f) = unsafeCoerce f

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

data Resolution i = Resolution {
    res_input  :: Map (Linked i)
  , res_output :: Map (Linked i)
  }

data Constraint i = In  (Item i)
                  | Out (Item i)

--------------------------------------------------------------------------------

type Item       i = Hide (Pair Name (Linked i))

type Mapping    i = Map (Node i)

type M i = Knot (Resolution i) (Constraint i) (State (Mapping i))

--------------------------------------------------------------------------------

node :: Name (S Symbol i a) -> M i (Node i (S Symbol i a))
node n =
  do out <- get
     case M.lookup n out of
       Nothing   -> error "Linker.node:lookup failed"
       Just node -> return node

resolve :: Name (S Symbol i a) -> M i (Link i a)
resolve n =
  do out <- asks res_output
     case M.lookup n out of
       Nothing           -> error "Linker.resolve:lookup failed"
       Just (Linked _ l) -> return l

input  :: Item i -> M i ()
input  i = tell [In i]

output :: Item i -> M i ()
output i = tell [Out i]

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

link' :: Ordered i -> M i ()
link' (Ordered sym) =
  do let out = name sym
     (Node n) <- node sym
     case n of
       (Repeat c) ->
         do output $ Hide $ Pair sym (Linked (Repeat c) (Link out))
       (Map f (Key s)) ->
         do inp <- resolve s
            output $ Hide $ Pair sym (Linked (Map f inp) (Link out))
       (Join (Key l) (Key r)) ->
         do inp_l@(Link l') <- resolve l
            inp_r@(Link r') <- resolve r
            output $ Hide $ Pair sym (Linked (Join inp_l inp_r) (Link (l', r')))
       (Left (Key l)) ->
         do inp_l@(Link l') <- resolve l
            output $ Hide $ Pair sym (Linked (Left inp_l) (Link (fst l')))
       (Right (Key r)) ->
         do inp_r@(Link r') <- resolve r
            output $ Hide $ Pair sym (Linked (Right inp_r) (Link (snd r')))
       (Delay d (Key s)) ->
         do inp <- resolve s
            output $ Hide $ Pair sym (Linked (Delay d inp) (Link out))

linker :: [Ordered i] -> Map (Node i) -> Map (Linked i)
linker order nodes = undefined
  where
    solve :: Solver (Resolution i) (Constraint i)
    solve constraints =
      let inputs  = [ i | In  i <- constraints]
          outputs = [ o | Out o <- constraints]
      in  Resolution {
              res_output = list outputs
            , res_input  = list  inputs
          }

    list :: [Item i] -> Map (Linked i)
    list = foldr (\(Hide (Pair n l)) m -> M.insert (ref n) l m) M.empty
      where ref = flip Data.Ref.Ref undefined

--------------------------------------------------------------------------------
