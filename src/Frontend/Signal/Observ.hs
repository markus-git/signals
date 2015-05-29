{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Signal.Observ where

import Core (IExp)
import Frontend.Signal (S, Signal(..), Sig(..), Symbol(..), U)
import Frontend.Stream (Stream, Str)
import Backend.Nested

import qualified Frontend.Signal as S
import qualified Frontend.Stream as Str

import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Data.Ref
import Data.Ref.Map (Map, Name)

import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right)

--------------------------------------------------------------------------------
-- * Graph representation of Signals
--------------------------------------------------------------------------------

data Key (i :: (* -> *) -> * -> *) (a :: *)
  where
    Key :: Name (S Symbol i a) -> Key i a

data Node i a
  where
    Node :: S Key i a -> Node i (S Symbol i a)

apa :: forall i a. Symbol i a -> Map Name -> Map (Node i) -> (Key i a, Map Name, Map (Node i))
apa (Symbol ref@(Ref name s)) names nodes
  | Just k <- M.lookup name names = (Key k, names, nodes)
  | otherwise = case s of
      (S.Repeat (s :: Stream i (IExp i b)))
        -> let smap    = S.Repeat s :: S Key i (Identity b)
               node    = Node smap  :: Node  i (S Symbol i a)
               nodes'  = M.insert ref node nodes
               names'  = M.insert ref name names
            in ( Key name
               , names'
               , nodes')
           
      (S.Map (f :: Stream i (U i b) -> Stream i (U i a))
             (a :: Symbol i b))
        -> let (a', names', nodes') = apa a names nodes
               smap    = S.Map f a' :: S Key i a
               node    = Node smap  :: Node  i (S Symbol i a)
               nodes'' = M.insert ref node nodes'
               names'' = M.insert ref name names'
            in ( Key name
               , names''
               , nodes'')

      (S.Join (l :: Symbol i b)
              (r :: Symbol i c))
        -> let (l', names',  nodes' ) = apa l names  nodes
               (r', names'', nodes'') = apa r names' nodes'
               sjoin    = S.Join l' r' :: S Key i a
               node     = Node sjoin   :: Node i (S Symbol i a)
               nodes''' = M.insert ref node nodes''
               names''' = M.insert ref name names''
            in ( Key name
               , names'''
               , nodes''')

      (S.Left (p :: Symbol i (a, b)))
        -> let (p', names', nodes') = apa p names nodes
               sleft   = S.Left p'  :: S Key i a
               node    = Node sleft :: Node i (S Symbol i a)
               nodes'' = M.insert ref node nodes'
               names'' = M.insert ref name names'
            in ( Key name
               , names''
               , nodes'')

      (S.Right (p :: Symbol i (b, a)))
        -> let (p', names', nodes') = apa p names nodes
               sright  = S.Right p'  :: S Key i a
               node    = Node sright :: Node i (S Symbol i a)
               nodes'' = M.insert ref node nodes'
               names'' = M.insert ref name names'
            in ( Key name
               , names''
               , nodes'')

      (S.Delay (e :: IExp i b) (a :: Symbol i (Identity b)))
        -> let (a', names', nodes') = apa a names nodes
               sdelay  = S.Delay e a' :: S Key i (Identity b)
               node    = Node sdelay  :: Node i (S Symbol i a)
               nodes'' = M.insert ref node nodes'
               names'' = M.insert ref name names'
            in ( Key name
               , names''
               , nodes'')

bepa :: forall i a. Symbol i a -> (Key i a, Map (Node i))
bepa sym = let (x, y, z) = apa sym M.empty M.empty in (x, z)

--------------------------------------------------------------------------------
