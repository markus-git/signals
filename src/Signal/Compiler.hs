{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler (compiler, compile) where

import Signal.Core               (S(..), Symbol(..), Sig, E(..))
import Signal.Core.Stream
import Signal.Core.Reify
import Signal.Core.Witness

import Signal.Compiler.Interface
import Signal.Compiler.Cycles
import Signal.Compiler.Sorter
import Signal.Compiler.Linker
import Signal.Compiler.Channels

import Control.Arrow             (first)
import Control.Monad.Identity    (Identity)
import Control.Monad.Reader      (ReaderT)
import Control.Monad.State       (State)
import Control.Monad.Operational.Compositional
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

import Language.VHDL          (Identifier)
import Language.Embedded.VHDL ( Kind
                              , Mode
                              , PredicateExp
                              , CompileExp
                              , SequentialCMD
                              , ConcurrentCMD
                              , HeaderCMD)
import qualified Language.VHDL          as VHDL
import qualified Language.Embedded.VHDL as HDL

import Prelude           hiding (read, Left, Right)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Compilation
--------------------------------------------------------------------------------

-- | Monad used for compilation
type Gen i = ReaderT (Channels i) (Program i)

read :: forall i a. CompileExp (IExp i) => Ident i a -> E i a
read (Ident i _ _) = dist (witness :: Wit i a) i
  where
    dist :: Wit i x -> Identifiers (S Symbol i x) -> E i x
    dist (WE) (Identified i) = HDL.varE i
    dist (WP l r) (u, v)     = (dist l u, dist r v)

write :: forall i a. (SequentialCMD (IExp i) :<: i) => Ident i a -> E i a -> Gen i ()
write (Ident i kind _) = dist (witness :: Wit i a) i
  where
    dist :: Wit i x -> Identifiers (S Symbol i x) -> E i x -> Gen i ()
    dist (WE) (Identified i) exp = CMS.lift $ case kind of
      HDL.Variable -> i HDL.==: exp
      HDL.Signal   -> i HDL.<== exp
    dist (WP l r) (u, v) (x, y) = dist l u x >> dist r v y

--------------------------------------------------------------------------------
-- **

comp'
  :: forall i.
     ( Compile       (IExp i)
     , CompileExp    (IExp i)
     , SequentialCMD (IExp i) :<: i
     , ConcurrentCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i)
  => Ordered i
  -> Gen     i ()
comp' (Ordered (Key name)) =
  do (Channel node out) <- CMR.asks (fromJust . RMap.lookup name)
     case node of
       Repeat c -> do
         declare out Nothing
         write out c
       Map f s -> do
         declare out Nothing
         write out (f $ read s)
       Delay d s -> do
         declare out (Just d)
         declare (global $ opposite out) Nothing
         write out (read s)
       Mux s cs -> do
         declare out Nothing
         env <- CMR.ask
         let (l, r)  = first (fmap literal) $ unzip cs
             choices = fmap (run env . write out . read) r
         CMS.lift $ HDL.switch (read s) (zip l choices) (Nothing)
       Var d -> do
          declare out Nothing
  where
    declare :: forall a. Ident i a -> Maybe (E i a) -> Gen i ()
    declare (Ident i k s) me = dist (witness :: Wit i a) i me
      where
        dist :: Wit i x -> Identifiers (S Symbol i x) -> Maybe (E i x) -> Gen i ()
        dist (WE) (Identified i) me = decl i me
        dist (WP l r) (u, v)     me = dist l u (fmap fst me) >> dist r v (fmap snd me)
        
        decl :: PredicateExp (IExp i) x => Identifier -> Maybe (IExp i x) -> Gen i ()
        decl ident exp = decl' ident k s exp

    global :: Ident i (Identity a) -> Ident i (Identity a)
    global (Ident is k _) = (Ident is k Global)

    run :: Channels i -> Gen i x -> Program i x
    run = flip CMR.runReaderT

decl' 
  :: ( PredicateExp  (IExp i) a
     , SequentialCMD (IExp i) :<: i
     , ConcurrentCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i)
  => Identifier
  -> Kind
  -> Scope
  -> Maybe (IExp i a)
  -> Gen i ()
decl' ident kind scope exp = CMS.lift $ case kind of
  HDL.Variable -> case scope of
    Header -> void $ HDL.signalPort ident HDL.In exp
    Local  ->        HDL.variableL  ident exp
  HDL.Signal   -> case scope of
    Header -> void $ HDL.signalPort ident HDL.Out exp
    Global ->        HDL.signalG    ident exp

--------------------------------------------------------------------------------
-- **

type Order i = [Ordered i]

compile'
  :: forall i a.
     ( Compile       (IExp i)
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) Bool
     , SequentialCMD (IExp i) :<: i
     , ConcurrentCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i)
  => Key      i (Identity a)  -- root
  -> Channels i               -- nodes
  -> Order    i               -- ordering
  -> Str      i a             -- output
compile' out channels order = Stream $ inArchitecture "arch" $
  do let inp@(delays, nodes) = split channels order
     clk <- HDL.clock
     run $ do
       inProcess "combinatorial" (sens inp) $
         do mapM_ comp' nodes
            mapM_ comp' delays
       unless (null delays) $
         inProcess "sequential" [clk] $
           when (rising clk) $
             mapM_ update delays
       exit out
  where
    run :: Gen i x -> Program i (Program i x)
    run = return . flip CMR.runReaderT (markRoot out channels)

    sens :: (Order i, Order i) -> [Identifier]
    sens (delays, nodes) =
        inputs channels nodes ++ concatMap ids delays
      where
        ids :: Ordered i -> [Identifier]
        ids (Ordered (Key name)) = case RMap.lookup name channels of
          Just (Channel (Delay {}) i) -> collect i

    when :: IExp i Bool -> Gen i () -> Gen i ()
    when exp = CMR.mapReaderT (HDL.when exp)

    update :: Ordered i -> Gen i ()
    update (Ordered (Key name)) = do
      (Channel (Delay {}) ident) <- CMR.asks (fromJust . RMap.lookup name)
      write (opposite ident) (read ident)

    exit :: Key i (Identity a) -> Gen i (IExp i a)
    exit (Key name) = do
      (Channel _ ident) <- CMR.asks (fromJust . RMap.lookup name)
      return $ read ident

    -- *** this is very hacky, as it assumes `IExp i` to be `HDL.Exp`
    rising :: CompileExp (IExp i) => Identifier -> IExp i Bool
    rising (VHDL.Ident i) = HDL.varE $ VHDL.Ident $ "rising_edge(" ++ i ++ ")"

--------------------------------------------------------------------------------

-- *** I don't like how it needs to lookup every ordered name
split :: Channels i -> Order i -> (Order i, Order i)
split c = partitionEithers . fmap sort
  where
    sort :: Ordered i -> Either (Ordered i) (Ordered i)
    sort ord@(Ordered (Key name)) = case RMap.lookup name c of
      Just (Channel (Delay {}) _) -> P.Left  ord
      _                           -> P.Right ord

inputs :: Channels i -> Order i -> [Identifier]
inputs c = concatMap vars
  where
    vars :: Ordered i -> [Identifier]
    vars ord@(Ordered (Key name)) = case RMap.lookup name c of
      Just (Channel (Var {}) i) -> collect i
      _                         -> []

-- *** This could easily be improved by not using lists internally
collect :: forall i a. Ident i a -> [Identifier]
collect (Ident is _ _) = dist (witness :: Wit i a) is
  where
    dist :: Wit i x -> Identifiers (S Symbol i x) -> [Identifier]
    dist (WE) (Identified i) = [i]
    dist (WP l r) (u, v)     = dist l u ++ dist r v

-- | Mark a key as root, giving it a signal kind and marking it as a port
markRoot :: Key i a -> Channels i -> Channels i
markRoot (Key name) = RMap.adjust update name
  where
    update :: Channel i a -> Channel i a
    update c = case c of
      Channel node (Ident i _ _) -> Channel node (Ident i HDL.Signal Header)

-- | Every delay has an `opposite` which is used in the combinatorial process
opposite :: Ident i (Identity a) -> Ident i (Identity a)
opposite (Ident (Identified (VHDL.Ident (i)))          k s) =
         (Ident (Identified (VHDL.Ident (i ++ "_in"))) k s)

--------------------------------------------------------------------------------

inProcess :: (ConcurrentCMD (IExp i) :<: i) => String -> [Identifier] -> Gen i () -> Gen i ()
inProcess name = CMR.mapReaderT . HDL.process name

inArchitecture :: (HeaderCMD (IExp i) :<: i) => String -> Program i (Program i a) -> Program i (Program i a)
inArchitecture name = fmap (HDL.architecture name)

--------------------------------------------------------------------------------
-- **

-- | Compile signal functions into stream functions
compiler
  :: ( Compile       (IExp i)
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) Bool
     , PredicateExp  (IExp i) a
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
       False ->
         let channels = fromLinks links
          in const $ compile' root channels order

--------------------------------------------------------------------------------
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
       False ->
         let channels = fromLinks links
          in compile' root channels order

--------------------------------------------------------------------------------
