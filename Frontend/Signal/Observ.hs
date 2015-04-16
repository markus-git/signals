{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Signal.Observ where

import Core
import Backend.Nested
import Frontend.Signal
import Frontend.Stream (Stream, Str)

import Control.Applicative hiding (Const)
import Data.Dynamic
import Data.Proxy
import Data.Reify

import Prelude hiding (Either(..))

--------------------------------------------------------------------------------
-- * Graph representation of Signals
--------------------------------------------------------------------------------

data TSignal (instr :: (* -> *) -> * -> *) (ref :: *)
  where
    TLambda :: ref -> ref -> TSignal instr ref

    TVar    :: TSignal instr ref
    
    TConst  :: Stream instr a -> TSignal instr ref

    TLift   :: (Stream instr a -> Stream instr b) -> ref -> TSignal instr ref

    TMap    :: (Tuple (IExp instr) a -> Tuple (IExp instr) b) -> ref -> TSignal instr ref

    TJoin   :: Suple (IExp instr) a -> Suple (IExp instr) b
            -> ref -> ref -> TSignal instr ref

    TLeft   :: ref -> TSignal instr ref

    TRight  :: ref -> TSignal instr ref

    TDelay  :: (IExp instr) a -> TSignal instr ref

type Node instr = TSignal instr Unique

--------------------------------------------------------------------------------
-- **
{-
instance MuRef (Sig instr a)
  where
    type DeRef (Sig instr a) = TSignal instr

    mapDeRef f = mapDeRef f . unSig
-}
instance MuRef (Signal instr a)
  where
    type DeRef (Signal instr a) = TSignal instr

    mapDeRef f node = case node of
--      (Const   s) -> pure $ TConst s
--      (Lift sf s) -> TLift sf <$> f s
--      (Map  tf s) -> TMap  tf <$> f s

      (Join  l r) -> TJoin  <$> f l <*> f r
        where
          sl = rep l
          sr = rep r
      
--      (Left  l)   -> TLeft  <$> f l
--      (Right r)   -> TRight <$> f r

instance (Typeable instr, Typeable a, Typeable b) => MuRef (Signal instr a -> Signal instr b)
  where
    type DeRef (Signal instr a -> Signal instr b) = TSignal instr

    mapDeRef f node =
      let (v, g) = let a = Var (toDyn node) in (a, node a)
      in TLambda <$> f v <*> f g

--------------------------------------------------------------------------------
-- ** 

edges :: Node instr -> [Unique]
edges node = case node of
  (TLambda a b) -> [a, b]
  (TLift _ a)   -> [a]
  (TMap  _ a)   -> [a]
  (TJoin a b)   -> [a, b]
  (TLeft a)     -> [a]
  (TRight a)    -> [a]
  _             -> []
