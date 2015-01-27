{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE Rank2Types                 #-}

module Frontend.SignalComp where

import           Frontend.Stream (Stream(..), Str)
import qualified Frontend.Stream as Str

import           Frontend.Signal ( Signal, Sig
                                 , Empty(..)
                                 , Struct(..)
                                 , TStruct(..), tleaf, tleft, tright, rep)
import qualified Frontend.Signal as Sig

import           Frontend.SignalObsv (TSignal(..), showTS, showP, edges)
import qualified Frontend.SignalObsv as SigO

import           Data.Map (Map, (!))
import qualified Data.Map as M

import           Core (CMD)
import qualified Core as C

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Operational

import           Control.Monad.Writer (WriterT, MonadWriter)
import qualified Control.Monad.Writer as CMW

import           Control.Monad.Reader (ReaderT, MonadReader, MonadTrans, lift)
import qualified Control.Monad.Reader as CMR

import           Control.Monad.State (StateT, State, MonadState, get, put, modify)
import qualified Control.Monad.State as CMS

import           Control.Monad.Identity (Identity, runIdentity)
import qualified Control.Monad.Identity as CMI

import Data.List (find, elem)
import Data.Maybe (fromJust)
import Data.Dynamic
import Data.Reify
import Data.Proxy
import Data.Typeable

import           Prelude
import qualified Prelude as P

---------------------------------------- : Testing
import qualified Expr as E
---------------------------------------- : End

--------------------------------------------------------------------------------
-- * Knot Monad
--------------------------------------------------------------------------------

newtype KnotT i x m a =
    KnotT { unKnotT :: ReaderT (Map i x) (WriterT [(i, x)] m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Map i x)
    , MonadWriter [(i, x)]
    )

type Knot i x = KnotT i x Identity

instance MonadTrans (KnotT i x)
  where
    lift = KnotT . lift . lift

class (Ord i, MonadReader (Map i x) m, MonadWriter [(i, x)] m) =>
    MonadKnot i x m | m -> i x
  where
    knot :: i -> m x
    (=:) :: i -> x -> m ()

instance (Ord i , MonadReader (Map i x) m, MonadWriter [(i, x)] m) =>
    MonadKnot i x m
  where
    knot i = CMR.asks (M.! i)
    i =: x = CMW.tell [(i, x)]

solve :: Ord i => [(i, x)] -> Map i x
solve = M.fromList

tie :: (Ord i, MonadFix m) => KnotT i x m a -> m (a, Map i x)
tie (KnotT knot) = mfix $ \ ~(a, solution) ->
  do ~(a, constraints) <- CMW.runWriterT $ CMR.runReaderT knot solution
     let solution = solve constraints
     return (a, solution)

--------------------------------------------------------------------------------
-- * Compiler
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Linker
--------------------------------------------------------------------------------

type Id  = String

type Ref = String

-- | ...
linker :: Graph (TSignal exp) -> Map Id Ref
linker (Graph nodes _) =
        snd
      . runIdentity
      . tie
      . sequence
      . fmap (link . showP)
      $ nodes

-- | ...
link :: (Id, TSignal exp Ref) -> Knot Id Ref ()
link (i, TLambda l r) =
  do l' <- knot l
     r' <- knot r
     i =: (l' ++ ". " ++ r')

link (i, TVar) =
  do i =: "input"

link (i, TConst _) =
  do i =: ('v' : i)

link (i, TLift _ s) =
  do knot s
     i =: ('v' : i)

link (i, TMap t t' _ s) =
  do s' <- asks t s   -- I'm not sure about this one,
     tells t s' i     -- both the output and input will be structs
     i =: ('v' : i)   -- but we only 'really' tell on the inputs

link (i, TZip t t' l r) =
  do sl <- asks t  l
     sr <- asks t' r
     tells (TPair t t') (TPair sl sr) i

link (i, TFst t l) =
  do sl <- asks t l
     tells (tleft t) (tleft sl) i

link (i, TSnd t r) =
  do sr <- asks t r
     tells (tright t) (tright sr) i

link (i, TDelay e s) =
  do s' <- knot s
     i =: ('d' : s')

--------------------------------------------------------------------------------

asks :: TStruct exp a -> String -> Knot Id Ref (TStruct exp a)
asks (TLeaf i)   s = knot s >>= return . TLeaf
asks (TPair l r) s =
  do l' <- asks l $ s ++ "_1"
     r' <- asks r $ s ++ "_2"
     return $ TPair l' r'

tells :: TStruct exp a
      -> TStruct exp a
      -> String
      -> Knot Id Ref ()
tells (TLeaf _)   t s = s =: (tleaf t)
tells (TPair l r) t s =
  do tells l (tleft  t) (s ++ "_1")
     tells r (tright t) (s ++ "_2")

--------------------------------------------------------------------------------
-- ** Sorter
--------------------------------------------------------------------------------

data Status = Visited Int | Visiting | Unvisited

type TNode exp = (Unique, TSignal exp Unique)

type Node  exp = (Status, TNode exp)

sorter :: Graph (TSignal exp) -> Map Unique Int
sorter (Graph nodes root) =
    M.map getOrder . snd . exec $ init >> find root >>= sort
  where
    exec s = case CMS.runState s (1, M.empty) of
               (True, state) -> state
               (False,    _) -> error "cycle in graph"

    init = mapM insert $ fmap ((,) Unvisited) nodes

    getOrder :: Node e -> Int
    getOrder (Visited i, _) = i

    insert :: Node e -> State (Int, Map Unique (Node e)) ()
    insert x@(_, (i, _)) = modify $ fmap $ M.insert i x

    find   :: Unique -> State (Int, Map Unique (Node e)) (TNode e)
    find i = get >>= return . snd . fromJust . M.lookup i . snd

sort :: TNode e -> State (Int, Map Unique (Node e)) Bool
sort (i, node) =
  do mark i Visiting
     b <- flip mapM (edges node) $ \e ->
            do (s, node') <- find e
               case s of
                 Unvisited -> sort node'
                 Visited _ -> return True
                 Visiting  -> return False
     c <- new
     mark i $ Visited c
     return $ if delayed node
               then True
               else and b
  where
    mark :: Unique -> Status -> State (Int, Map Unique (Node e)) ()
    mark i s = modify $ fmap $ M.adjust (first $ const s) i

    find :: Unique -> State (Int, Map Unique (Node e)) (Node e)
    find i = get >>= return . (! i) . snd

    new :: State (Int, Map Unique (Node e)) Int
    new = do (i, m) <- get
             put (i + 1, m)
             return i

    delayed :: TSignal e a -> Bool
    delayed (TDelay _ _) = True
    delayed _            = False

--------------------------------------------------------------------------------
-- *

type LMap = Map Id Ref

type OMap = Map Unique Int

comp :: TNode exp -> Map Id Ref -> Program (CMD exp) (exp b) -> Program (CMD exp) a
comp (i, TVar) m input = undefined

filterMaps :: Graph (TSignal e) -> (LMap, OMap) -> (LMap, OMap)
filterMaps (Graph nodes _) (lm, om) = (lm', om')
  where
    om' = M.filterWithKey (\k _ -> not $ isnop k) om
    lm' = M.filterWithKey (\k _ -> not $ isnop (read [head k])) lm

    isnop :: Unique -> Bool
    isnop i = nop $ snd $ fromJust $ find ((==i) . fst) nodes

    nop :: TSignal e a -> Bool
    nop (TZip    {}) = True
    nop (TFst    {}) = True
    nop (TSnd    {}) = True
    nop (TLambda {}) = True
    nop _            = False

--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

type S  = Sig E.Expr Float
type TS = TSignal E.Expr

sig :: S -> S
sig s = s + 0

tsig :: IO (Graph TS)
tsig = reifyGraph sig

test :: IO ()
test = do
  g@(Graph nodes root) <- tsig
  putStrLn $ show g
  let m = linker g
  putStrLn $ show m
  let s = sorter g
  putStrLn $ show s
  putStrLn "==========="
  let (m', s') = filterMaps g (m, s)
  putStrLn $ show m'
  putStrLn $ show s'
  putStrLn "==========="
