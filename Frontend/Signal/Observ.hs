{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Signal.Observ where

import Core
import Backend.Nested
import Frontend.Signal (Signal, Sig, F, Wit, wit)
import Frontend.Stream (Stream, Str)

import qualified Frontend.Signal as S

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
    Const  :: ( Typeable a, VarPred e a
              , e ~ IExp instr
              )
           => Stream instr (e a) -> Obsv i ref
              
    Map    :: ( Typeable a --, VarPred e a
              , Typeable b --, VarPred e b
              , e ~ IExp instr
              )
           => (Stream instr (F i a) -> Stream instr (F i b))
           -> Wit a -> Wit b -> ref -> Obsv i ref

    Join   :: ( Typeable a
              , Typeable b
              )
           => Wit a -> Wit b -> ref -> ref -> Obsv i ref

    Left   :: ( Typeable a
              , Typeable b
              )
           => Wit (a, b) -> ref -> Obsv i ref

    Right  :: ( Typeable a
              , Typeable b
              )
           => Wit (a, b) -> ref -> Obsv i ref

    Var    :: Typeable a => Wit a -> Obsv i ref
    
    Lambda :: ref -> ref -> Obsv i ref
    
type Node instr = Obsv instr Unique

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

instance forall i a b. MuRef (Signal i a)
  where
    type DeRef (Signal i a) = Obsv i

    mapDeRef ref node = case node of
      (S.Var   _) -> pure $ Var (wit :: Wit a)
      (S.Const s) -> pure $ Const s
      
      (S.Map (f :: Stream i (F i b) -> Stream i (F i a)) (s :: Signal i b))
        -> Map f (wit :: Wit b) (wit :: Wit a) <$> ref s
        
{-
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
-}
{-
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
