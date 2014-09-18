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
import Data.List     (mapAccumL)
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
                  :: Program (CMD Expr) (Ref (Expr (x, y)))

            -- todo: remove
          apa <- getRef sd
          setRef r $ fstE apa

          return $ toDyn r

        (TDelay  v s) -> todo
        (TSample n s) -> todo

        (TMarrV r)   -> do
          let buff = fromJust $ M.lookup r buffers
          v <- input
          putBuff buff v
          r <- newRef v
          return $ toDyn r
        (TMarrD r i) -> do
          let buff = fromJust $ M.lookup r buffers
          v <- getBuff buff i
          r <- newRef v
          return $ toDyn r

    findNode :: Unique -> Node
    findNode u = fromJust $ find ((P.==) u . P.fst) gnodes

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

instance Show (Buffer a) where show _ = "buffer"

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

type Chain = [Node]

-- todo : remove one element buffers
--      , support mult. refs. to same node
find_delay_chains
  :: Typeable a
  => Graph SigTree
  -> Program (CMD Expr) (Map Unique (Buffer a), Graph SigTree)
find_delay_chains (Graph gnodes groot) = do
    (chains, buffers) <- fmap P.unzip $ buffer $ find gnodes
    let nodes = update chains gnodes
    let maps  = M.fromList $ P.zip (P.map (P.fst . P.head) chains) buffers
    return (maps, Graph nodes groot)
  where
    -- ^ Find all delay chains in the graph
    find :: [Node] -> [Chain]
    find nodes = fmap (flip chain delays) vars
      where
        chain :: Node -> [Node] -> Chain
        chain u@(i, _) xs = u : go i xs
          where
            go _ [] = []
            go u (y@(i, TDelay _ r):ys)
              | u == r    = y : go i xs
              | otherwise = go u ys

        vars   = [x | x@(_, TVar)       <- nodes]
        delays = [x | x@(_, TDelay _ _) <- nodes]

      -- ^ Create buffers from chains and update nodes
    buffer
      :: Typeable a => [Chain] -> Program (CMD Expr) [(Chain, Buffer a)]
    buffer chains = P.sequence $ fmap (create . update) chains
      where
        update
          :: Typeable a
          => Chain -> (Chain, [Expr a])
        update ((u, TVar):xs) =
          let (chain, delays) = P.unzip $ P.snd $ mapAccumL f 1 xs
           in ((u, TMarrV u) : chain, delays)
          where
            f n (i, TDelay a _) = (n+1, ((i, TMarrD u n), fromJust $ cast a))

        create
          :: Typeable a
          => (Chain, [Expr a]) -> Program (CMD Expr) (Chain, Buffer a)
        create (ns, as) = do
          buff <- newBuff (Val $ length as) (head as) -- todo: create init function
          return (ns, buff)

    -- ^ Replace old nodes in graph with those from the updated version
    update :: [Chain] -> [Node] -> [Node]
    update xs nodes = P.foldr (P.flip (P.foldr replace)) nodes xs
      where
        replace :: Node -> [Node] -> [Node]
        replace _ [] = []
        replace y (x:xs)
          | P.fst y == P.fst x = y : xs
          | otherwise          = x : replace y xs
