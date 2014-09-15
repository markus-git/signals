{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}

module Frontend.SignalComp where

import Core
import Expr hiding (Var)
import Interpretation

import Backend.C

import Frontend.Signal
import Frontend.SignalObsv
import qualified Frontend.Stream as S

import Control.Applicative
import Control.Monad.Operational
import Data.Dynamic
import Data.Maybe    (fromJust)
import Data.Foldable (find)
import Data.Reify
import Data.Proxy

import Data.Map (Map)
import qualified Data.Map as M

import Prelude hiding (const, fst, snd, map, repeat, zipWith)
import qualified Prelude as P

import Unsafe.Coerce -- todo: remove

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

runSignal
  :: forall a b. (Typeable a, Typeable b)
  => (   Signal (Expr a)
      -> Signal (Expr b))
  -> IO (   Program (CMD Expr) (Expr a)
         -> Program (CMD Expr) (Expr b))
runSignal f
  = reifyGraph f >>= return . compGraph

--------------------------------------------------------------------------------
-- **

-- instance Typeable Ptr

-- instance Typeable1 Ref

type Node = (Unique, SigTree Unique)

compGraph :: forall a b. (Typeable a, Typeable b)
          => Graph SigTree
          -> Program (CMD Expr) (Expr a)
          -> Program (CMD Expr) (Expr b)
compGraph graph@(Graph gnodes groot) input =
  do x <- castNode groot M.empty :: Program (CMD Expr) (Ref (Expr b))
     getRef x
  where
    compNode :: Node -> Map Unique Dynamic -> Program (CMD Expr) Dynamic
    compNode (i, node) nodes
      | Just dyn <- M.lookup i nodes = return dyn
      | otherwise = case node of
        (TVar)        -> input >>= newRef >>= return . toDyn
        (TLambda (p :: Proxy (x, y)) v s) -> do
          r  <- initRef :: Program (CMD Expr) (Ref (Expr x))
          vd <- loadNode v (addNode i (toDyn r) nodes)
          sd <- loadNode s (addNode v vd        nodes)
                 -- :: Program (CMD Expr) (Ref (Expr x))

            -- todo: remove
          let sd' :: Ref (Expr x)
              sd' = case fromDynamic sd of
                           Just x  -> x
                           Nothing -> error $ "lambda"
                                        ++ "\n\t types:" ++ show (toDyn p)
                                        ++ "\n\t vd: " ++ show vd
                                        ++ "\n\t sd: " ++ show sd
                                        ++ "\n\t r:  " ++ show (typeOf r)
          apa <- getRef sd'
          setRef r apa

          return $ toDyn r

        (TConst s) -> do
          sd <- S.runStream s
          r  <- newRef sd
          return $ toDyn r
        (TLift f s) -> do
          r  <- initRef
          sd <- castNode s (addNode i (toDyn r) nodes)

            -- todo: remove
          apa <- S.runStream . f $ S.Stream $ return $ getRef sd
          setRef r apa

          return $ toDyn r

        (TZip (p :: Proxy (x, y)) s u) -> do
          r  <- initRef
          sd <- castNode s (addNode i (toDyn r)  nodes)
                  :: Program (CMD Expr) (Ref (Expr x))
          ud <- castNode u (addNode i (toDyn sd) nodes)
                  :: Program (CMD Expr) (Ref (Expr y))
          sv <- getRef sd
          uv <- getRef ud
          setRef r $ tupE sv uv
          return $ toDyn r
        (TFst (p :: Proxy (x, y)) s) -> do
          r  <- initRef
          sd <- castNode s (addNode i (toDyn r) nodes)
                  :: Program (CMD Expr) (Ref (Expr x))

            -- todo: remove
          apa <- getRef sd
          setRef r apa

          return $ toDyn r

    findNode :: Unique -> Node
    findNode u = fromJust $ find ((P.==) u . P.fst) gnodes

    addNode :: Unique -> Dynamic -> Map Unique Dynamic -> Map Unique Dynamic
    addNode = M.insertWith (P.flip P.const)

    loadNode :: Unique -> Map Unique Dynamic -> Program (CMD Expr) Dynamic
    loadNode u m = P.flip compNode m $ findNode u

    castNode :: Typeable n => Unique -> Map Unique Dynamic -> Program (CMD Expr) n
    castNode u m = (fromJust . fromDynamic) <$> loadNode u m
