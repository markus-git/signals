{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Signal.Observ where

import Core
import Backend.Nested
import Frontend.Signal
import Frontend.Stream (Stream, Str)

import Control.Applicative hiding (Const)
import Data.Dynamic
import Data.Functor.Identity
import Data.Proxy
import Data.Reify

import Prelude hiding (Either(..))

--------------------------------------------------------------------------------
-- * Graph representation of Signals
--------------------------------------------------------------------------------

data Obsv (instr :: (* -> *) -> * -> *) (ref :: *)
  where
    OConst :: ( Typeable a, VarPred e a
              , e ~ IExp instr
              )
              => Stream instr (F e (Identity a))
              -> ref
              -> Obsv i ref
              
    OMap   :: ( Typeable a
              , Typeable b
              , e ~ IExp instr
              )
              => (Stream instr (F e a) -> Stream instr (F e b))
              -> ref
              -> ref
              -> Obsv i ref
{-
    TLambda :: ref -> ref -> TSignal instr ref

    TVar    :: (Typeable a)
            => Suple instr a -> TSignal instr ref
               
    TConst  :: (Typeable a, VarPred (IExp instr) a)
            => Stream instr (IExp instr a) -> TSignal instr ref

    TLift   :: (Typeable a, Typeable b, VarPred (IExp instr) a, VarPred (IExp instr) b)
            => (Stream instr (IExp instr a) -> Stream instr (IExp instr b))
            -> ref
            -> TSignal instr ref

    TDelay  :: (Typeable a, VarPred (IExp instr) a)
            => IExp instr a
            -> ref
            -> TSignal instr ref

    TMap    :: (Typeable a, Typeable b)
            =>  Suple instr a -> Suple instr b
            -> (Tuple instr a -> Tuple instr b)
            -> ref
            -> TSignal instr ref

    TJoin   :: (Typeable a, Typeable b)
            => Suple instr a -> Suple instr b
            -> ref
            -> ref
            -> TSignal instr ref

    TLeft   :: (Typeable a, Typeable b)
            => Suple instr (a, b)
            -> ref
            -> TSignal instr ref

    TRight  :: (Typeable a, Typeable b)
            => Suple instr (a, b)
            -> ref
            -> TSignal instr ref
-}
--type Node instr = TSignal instr Unique

--------------------------------------------------------------------------------
-- **
{-
instance MuRef (Signal instr a)
  where
    type DeRef (Signal instr a) = TSignal instr

    mapDeRef f node = case node of
      (Var _) -> pure $ TVar (rep (undefined :: Signal instr a))
        
      (Const   s) -> pure $ TConst s
      (Lift sf s) -> TLift sf <$> f s
      (Delay a s) -> TDelay a <$> f s

      (Map (tf :: Tuple instr a1 -> Tuple instr a) s)
        -> let sl = rep (undefined :: Signal instr a1)
               sr = rep (undefined :: Signal instr a)
            in TMap sl sr tf <$> f s


      (Join l r) -> TJoin  (rep l) (rep r) <$> f l <*> f r
      (Left l)   -> TLeft  (rep l) <$> f l
      (Right  r) -> TRight (rep r) <$> f r

-- todo : we dont need typeable of instr, as it doesn't contribute to a safer var construction
instance (Instr a ~ instr, Rep a, Typeable instr, Typeable a, Typeable b) =>
    MuRef (Signal instr a -> Signal instr b)
  where
    type DeRef (Signal instr a -> Signal instr b) = TSignal instr

    mapDeRef f node =
      let (v, g) = let a = Var (toDyn node) in (a, node a)
      in TLambda <$> f v <*> f g
-}
--------------------------------------------------------------------------------
{-
instance MuRef (Sig instr a)
  where
    type DeRef (Sig instr a) = TSignal instr

    mapDeRef f = mapDeRef f . unSig

instance (VarPred (IExp instr) a, Typeable instr, Typeable a, Typeable b) =>
    MuRef (Sig instr a -> Sig instr b)
  where
    type DeRef (Sig instr a -> Sig instr b) = TSignal instr

    mapDeRef f sf = mapDeRef f (unSig . sf . Sig)
-}
--------------------------------------------------------------------------------
-- ** 
{-
edges :: Node instr -> [Unique]
edges node = case node of
  (TLambda a b)     -> [a, b]
  (TLift _ a)       -> [a]
  (TMap  _ _ _ a)   -> [a]
  (TJoin _ _ a b)   -> [a, b]
  (TLeft _ a)       -> [a]
  (TRight _ a)      -> [a]
  _                 -> []
-}
