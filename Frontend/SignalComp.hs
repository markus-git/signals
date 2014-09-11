{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.SignalComp where

import Core
import Expr hiding (Var)

import Frontend.Signal
import Frontend.SignalObsv

import Control.Monad.Operational
import Data.Dynamic
import Data.Reify

import Data.Map (Map)
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

toCore :: forall a b. (Typeable a, Typeable b)
       => (Signal (Expr a) -> Signal (Expr b))
       -> IO (Program (CMD Expr) (Expr a) ->
              Program (CMD Expr) (Expr b))
toCore f = reifyGraph f >>= return . compGraph

--------------------------------------------------------------------------------
-- **

type Node = (Unique, SigTree Unique)

compGraph :: forall a b. (Typeable a, Typeable b)
          => Graph SigTree
          -> Program (CMD Expr) (Expr a)
          -> Program (CMD Expr) (Expr b)
compGraph graph@(Graph gnodes groot) input = undefined
  where
    compNode :: Node -> Map Unique Dynamic -> Program (CMD Expr) Dynamic
    compNode = undefined

    findNode :: Unique -> Node
    findNode u = undefined

    loadNode :: Unique -> Map Unique Dynamic -> Map Unique Dynamic
    loadNode u m = undefined

    addNode :: Unique -> Dynamic -> Map Unique Dynamic -> Map Unique Dynamic
    addNode = undefined

    castNode :: Typeable n => Unique -> Map Unique Dynamic -> Program (CMD Expr) n
    castNode = undefined
