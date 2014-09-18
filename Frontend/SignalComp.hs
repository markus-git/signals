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
import Data.List     (mapAccumR)
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
-- * Compilation of Signals
--------------------------------------------------------------------------------

type Node = (Unique, SigTree Unique)

runSignal
  :: forall a b. (Typeable a, Typeable b)
  => (   Signal (Expr a)
      -> Signal (Expr b))
  -> IO (   Program (CMD Expr) (Expr a)
         -> Program (CMD Expr) (Expr b))
runSignal f = do
  g <- reifyGraph f
  return $ \input -> do
    (m, g') <- find_delay_chains g
    compGraph g' m input

--------------------------------------------------------------------------------
-- ** Compiling

compGraph :: forall a b. (Typeable a, Typeable b)
          => Graph SigTree
          -> Map Unique (Buffer a)
          -> Program (CMD Expr) (Expr a)
          -> Program (CMD Expr) (Expr b)
compGraph graph@(Graph gnodes groot) buffers input =
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
          sd <- castNode s (addNode v vd        nodes)
                 -- :: Program (CMD Expr) (Ref (Expr x))

            -- todo: remove
          apa <- getRef sd
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

        (TDelay v s) -> do
          undefined
        (TMarrV r) -> do
          let buff = case (M.lookup r buffers) of
                       Just x  -> x
                       Nothing -> error "TMarrV: Missing buffer!"
          v <- input
          putBuff buff v
          r <- newRef v
          return $ toDyn r
        (TMarrD r i) -> do
          let buff = case (M.lookup r buffers) of
                       Just x  -> x
                       Nothing -> error "TMarrD: Missing buffer!"
          v <- getBuff buff i
          r <- newRef v
          return $ toDyn r

    findNode :: Unique -> Node
    findNode u = case (find ((P.==) u . P.fst) gnodes) of
                   Just x  -> x
                   Nothing -> error $ "couldn't find node " ++ show u
                                    ++ "in graph " ++ show gnodes

    addNode :: Unique -> Dynamic -> Map Unique Dynamic -> Map Unique Dynamic
    addNode = M.insertWith (P.flip P.const)

    loadNode :: Unique -> Map Unique Dynamic -> Program (CMD Expr) Dynamic
    loadNode u m = P.flip compNode m $ findNode u

    castNode :: Typeable n => Unique -> Map Unique Dynamic -> Program (CMD Expr) n
    castNode u m = (fromJust . fromDynamic) <$> loadNode u m


--------------------------------------------------------------------------------
-- * Optimization
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Buffers

-- |
data Buffer a = Buffer
  { getBuff :: Expr Int -> Program (CMD Expr) (Expr a)
  , putBuff :: Expr a   -> Program (CMD Expr) ()
  }

newBuff :: Expr Int -> Expr a -> Program (CMD Expr) (Buffer a)
newBuff size init = do
  arr <- newArr size init
  ir  <- newRef 0
  let get j = do
        i <- getRef ir
        getArr ((size + i + j - 1) `mod` size) arr
  let put a = do
        i <- getRef ir
        setRef ir ((i + 1) `mod` size)
        setArr i a arr
  return $ Buffer get put

getBuffer :: Expr Int -> Buffer a -> Program (CMD Expr) (Expr a)
getBuffer = flip getBuff

putBuffer :: Expr a -> Buffer a -> Program (CMD Expr) ()
putBuffer = flip putBuff

--------------------------------------------------------------------------------
-- ** Delay chains

-- todo : remove one element buffers
--      , support mult. refs. to same node
find_delay_chains
  :: Typeable a
  => Graph SigTree
  -> Program (CMD Expr) (Map Unique (Buffer a), Graph SigTree)
find_delay_chains (Graph nodes root) =
  do chains'   <- create_buffers $ find_chains nodes
     let nodes' = update_graph (fmap (\(x,y,z) -> y)     chains') nodes
         map'   = M.fromList   (fmap (\(x,y,z) -> (x,z)) chains')
     return (map', Graph nodes' root)
  where
    -- ^ Find all delay chains in the graph
    find_chains :: [Node] -> [[Node]]
    find_chains nodes = fmap (flip chain delays) vars
      where
        chain :: Node -> [Node] -> [Node]
        chain u@(i, _) xs = u : go i xs
          where
            go _ [] = []
            go u (y@(i, TDelay _ r):ys)
              | u == r    = y : go i xs
              | otherwise = go u ys

        vars   = [x | x@(_, TVar)       <- nodes]
        delays = [x | x@(_, TDelay _ _) <- nodes]

    create_buffers
      :: Typeable a
      => [[Node]] -> Program (CMD Expr) ([(Unique, [Node], Buffer a)])
    create_buffers chains = P.sequence $ fmap buffer $ fmap update chains
      where
        update :: Typeable a => [Node] -> (Unique, [Node], [Expr a])
        update ((u, _):xs) = uncurry ((,,) u) $ P.unzip $ P.snd $ mapAccumR f 1 xs
          where
            f :: Typeable a => Expr Int -> Node -> (Expr Int, (Node, Expr a))
            f n (i, TDelay a _) = (n + 1, ( (i, TMarrD u n)
                                          , case cast a of
                                              Just x  -> x
                                              Nothing -> error "casting in update"))

        buffer
          :: Typeable a => (Unique, [Node], [Expr a])
          -> Program (CMD Expr) (Unique, [Node], Buffer a)
        buffer (u, ns, []) = error (show u)
        buffer (u, ns, as) = do
          buff <- newBuff (Val $ length as) (head as)
          P.sequence $ fmap (putBuff buff) as -- todo: create init function
          return (u, ns, buff)

    -- ^ Replace old nodes in graph with those from the updated version
    update_graph :: [[Node]] -> [Node] -> [Node]
    update_graph xs nodes = P.foldr (P.flip (P.foldr replace)) nodes xs
      where
        replace :: Node -> [Node] -> [Node]
        replace _ [] = []
        replace y (x:xs)
          | P.fst y == P.fst x = y : xs
          | otherwise          = x : replace y xs
