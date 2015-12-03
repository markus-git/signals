{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler {-(compiler, compile)-} where

import Signal.Core (S(..), Symbol(..), Sig, E(..))
import Signal.Core.Stream
import Signal.Core.Reify
import Signal.Core.Witness

import Signal.Compiler.Interface
import Signal.Compiler.Cycles
import Signal.Compiler.Sorter
import Signal.Compiler.Linker
import Signal.Compiler.Channels (Channels)
import qualified Signal.Compiler.Channels as Chan

import Control.Arrow             (first)
import Control.Monad.Identity    (Identity)
import Control.Monad.Reader      (ReaderT)
import Control.Monad.State       (State)
import Control.Monad.Operational.Higher
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad.Reader   as CMR
import qualified Control.Monad.State    as CMS

import Data.Either  (partitionEithers)
import Data.Maybe   (fromJust)
import Data.Typeable
import Data.IntMap  (IntMap)
import Data.Ref
import Data.Ref.Map (Name, Map)
import qualified Data.IntMap  as IMap
import qualified Data.Ref.Map as RMap

import Language.VHDL (Identifier)
import Language.Embedded.VHDL (Mode, PredicateExp, CompileExp, SequentialCMD, ConcurrentCMD, HeaderCMD)
import Language.Embedded.VHDL.Expression.Type (Kind)
import qualified Language.VHDL                          as VHDL
import qualified Language.Embedded.VHDL                 as HDL
import qualified Language.Embedded.VHDL.Expression.Type as HDL

import Prelude hiding (read, Left, Right)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Compilation
--------------------------------------------------------------------------------

-- | Monad used for compilation
type Gen i = ReaderT Channels (Program i)

-- exploits the behaviour of HDL.genSym and HDL.compiler.
identifier :: (CompileExp exp, PredicateExp exp a) => Identifier -> exp a
identifier (VHDL.Ident ('v':i)) = HDL.varE (P.read i :: Integer)

read :: forall i a. CompileExp (IExp i) => Link i a -> Gen i (E i a)
read (Link names) = dist (witness :: Wit i a) names
  where
    dist :: Wit i x -> Names (S Symbol i x) -> Gen i (E i x)
    dist (WE) (name) =
      do chan <- CMR.asks (Chan.lookup name)
         return $ case chan of
           Nothing -> error "missing channel in read!"
           Just (Chan.Channel i _) -> identifier i
    dist (WP l r) (u, v) =
      do x <- dist l u
         y <- dist r v
         return (x, y)

write :: forall i a. (SequentialCMD (IExp i) :<: i) => Link i a -> E i a -> Gen i ()
write (Link names) = dist (witness :: Wit i a) names
  where
    dist :: Wit i x -> Names (S Symbol i x) -> E i x -> Gen i ()
    dist (WE) (name) (exp) =
      do chan <- CMR.asks (Chan.lookup name)
         CMR.lift $ case chan of
           Nothing -> error "missing channel in write!"
           Just (Chan.Channel i HDL.Signal)   -> i HDL.<== exp
           Just (Chan.Channel i HDL.Variable) -> i HDL.==: exp
    dist (WP l r) (u, v) (x, y) =
      do dist l u x
         dist r v y

swap :: Link i (Identity a) -> Link i (Identity a)
swap (Link name) = Link (other name)

--------------------------------------------------------------------------------
-- **

compileSym
  :: forall i a.
     ( Compile       (IExp i)
     , CompileExp    (IExp i)
     , SequentialCMD (IExp i) :<: i
     , ConcurrentCMD (IExp i) :<: i)
  => Linked i a
  -> Gen    i ()
compileSym (Linked sym out) = case sym of
  Map f s -> do
    x <- read s
    write out (f x)
  Delay d s -> do
    x <- read s
    write (swap out) x
  Mux s cs -> do
    env <- CMR.ask
    let (ls, rs) = unzip cs
        literals = fmap literal ls
        choices  = flip fmap rs $ \r -> flip CMR.runReaderT env $
          do x <- read r
             write out x
    x <- read s
    CMS.lift $ HDL.switch x (zip literals choices) Nothing
  _ -> return ()

updateSym
  :: forall i a.
     ( CompileExp    (IExp i)
     , SequentialCMD (IExp i) :<: i)
  => Linked i a
  -> Gen    i ()
updateSym (Linked sym out) = case sym of
  Delay d s -> do
    x <- read (swap out)
    write out x
  _ -> return ()

--------------------------------------------------------------------------------
-- **

compileSig
  :: forall i a.
     ( Compile       (IExp i)
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) Bool
     , SequentialCMD (IExp i) :<: i
     , ConcurrentCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i)
  => Key    i (Identity a) -- ^ Root name
  -> Links  i              -- ^ Links between names and their signal constructors
  -> Orders i              -- ^ Names sorted in a topological ordering
  -> Str    i a
compileSig key links ords = Stream . inArchitecture "arch" $
  do let (delays, nodes) = filterDelays links ords
     signals <- Chan.declareSignals key links
     clock   <- HDL.signalPort HDL.In (Nothing :: Maybe (IExp i Bool))
     return $ do
       let sl = sensitivities delays signals
       inProcess "combinatorial" sl $ do
         channels <- (signals `Chan.with`) <$> Chan.declareVariables key links
         mapM_ (`cmp` channels) nodes 
         mapM_ (`cmp` channels) delays
       inProcess "sequential" [clock] $ do
         mapM_ (`upd` signals) delays
       return $ error "!"
  where
    run :: Channels -> Gen i x -> Program i (Program i x)
    run chan = return . flip CMR.runReaderT chan

    cmp :: Ix i -> Channels -> Program i ()
    cmp (Hide x) = CMR.runReaderT (compileSym x)

    upd :: Ix i -> Channels -> Program i ()
    upd (Hide x) = CMR.runReaderT (updateSym x)

    exit :: Key i (Identity a) -> Channels -> Program i (IExp i a)
    exit (Key name) channels = case RMap.lookup name links of
      Just (Linked _ link) -> CMR.runReaderT (read link) channels
      Nothing              -> error "huh?"

--------------------------------------------------------------------------------

type Ix i = Hide (Linked i)

filterDelays :: Links i -> Orders i -> ([Ix i], [Ix i])
filterDelays links = partitionEithers . fmap eitherDelay
  where
    eitherDelay :: Ordered i -> Either (Ix i) (Ix i)
    eitherDelay (Ordered (Key name)) = case RMap.lookup name links of
      Just l@(Linked (Delay {}) n) -> P.Left  (Hide l)
      Just l@(Linked _          n) -> P.Right (Hide l)

sensitivities :: [Ix i] -> Channels -> [Identifier]
sensitivities ix channels =
    concatMap (\(Hide (Linked _ link)) -> fetch link) ix
  where
    fetch :: forall i a. Link i a -> [Identifier]
    fetch (Link name) = dist (witness :: Wit i a) name
      where
        dist :: Wit i x -> Names (S Symbol i x) -> [Identifier]
        dist (WP l r) (u, v) = dist l u ++ dist r v
        dist (WE)     (name) = case Chan.lookup name channels of
          Just (Chan.Channel i _) -> [i]
          Nothing                 -> [ ]

inProcess :: (ConcurrentCMD (IExp i) :<: i) => String -> [Identifier] -> Program i () -> Program i ()
inProcess = HDL.process

inArchitecture :: (HeaderCMD (IExp i) :<: i) => String -> Program i (Program i a) -> Program i (Program i a)
inArchitecture name = return . HDL.architecture name . join

--------------------------------------------------------------------------------
-- **

-- | Compile signal functions into stream functions
compiler
  :: ( Compile       (IExp i)
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) Bool
     , PredicateExp  (IExp i) a
     , PredicateExp  (IExp i) b
     , SequentialCMD (IExp i) :<: i
     , ConcurrentCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , Typeable a
     , Typeable b
     , Typeable i)
  => (Sig i a -> Sig i b) -> IO (Str i a -> Str i b)
compiler f =
  do (root, nodes) <- freify f
     let order = sorter root  nodes
         cycle = cycles root  nodes
         links = linker order nodes
     return $ case cycle of
       True  -> error "signal compiler: found cycle"
       False -> const $ compileSig root links order

-- | Compile signals into streams
compile
  :: ( Compile       (IExp i)
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) Bool
     , PredicateExp  (IExp i) a
     , SequentialCMD (IExp i) :<: i
     , ConcurrentCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , Typeable a)
  => Sig i a -> IO (Str i a)
compile f =
  do (root, nodes) <- reify f
     let order = sorter root  nodes
         cycle = cycles root  nodes
         links = linker order nodes
     return $ case cycle of
       True  -> error "signal compiler: found cycle"
       False -> compileSig root links order

--------------------------------------------------------------------------------
