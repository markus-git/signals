{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler (compiler) where

import Signal.Core hiding (lift)
import Signal.Core.Stream
import Signal.Core.Witness
import Signal.Core.Reify
import qualified Signal.Core        as S
import qualified Signal.Core.Stream as Str
import Signal.Compiler.Cycles
import Signal.Compiler.Linker
import Signal.Compiler.Sorter

import Control.Monad.Operational.Compositional
import Control.Arrow    (first, second)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State  (StateT, State, modify, gets, lift)
import Control.Monad.Identity
import qualified Control.Monad.Reader as CMR
import qualified Control.Monad.State  as CMS
import Data.Either      (partitionEithers)
import Data.Maybe       (catMaybes)
import Data.List        (sortBy, delete)
import Data.Traversable (traverse)
import Data.Typeable
import Data.Function    (on)
import Data.Hashable
import Data.Constraint
import Data.Ref
import Data.Ref.Map     (Name)
import qualified Data.IntMap  as Map
import qualified Data.Ref.Map as Rim
import Language.VHDL (Identifier(..))
import Language.Embedded.VHDL hiding (name)
import qualified Language.Embedded.VHDL as E
import System.Mem.StableName (eqStableName)

import Prelude hiding (read, lookup, Left, Right)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Channels
--------------------------------------------------------------------------------

data Channels = Channels {
    chan_nodes  :: Map.IntMap (Identifier, Kind)
  , chan_delays :: Map.IntMap (Identifier, Identifier) -- delays always have kind 'M.Signal'
  }

emptyChannels :: Channels
emptyChannels = Channels (Map.empty) (Map.empty)

--------------------------------------------------------------------------------
-- ** Lookup / Insert

lookupNode  :: Named (S Symbol i (Identity a)) -> Channels -> (Identifier, Kind)
lookupNode n c = (Map.!) (chan_nodes c) (hash n)

lookupDelay :: Named (S Symbol i (Identity a)) -> Channels -> (Identifier, Identifier)
lookupDelay n c = (Map.!) (chan_delays c) (hash n)

insertNode  :: Named (S Symbol i (Identity a)) -> Identifier -> Kind -> Channels -> Channels
insertNode n i k (Channels ns ds) = Channels (Map.insert (hash n) (i, k) ns) ds

insertDelay :: Named (S Symbol i (Identity a)) -> Identifier -> Identifier -> Channels -> Channels
insertDelay n ni no (Channels ns ds) = Channels ns (Map.insert (hash n) (ni, no) ds)

markNode    :: Named (S Symbol i (Identity a)) -> Kind -> Channels -> Channels
markNode n k (Channels ns ds) = Channels (Map.adjust (second (const k)) (hash n) ns) ds

--------------------------------------------------------------------------------
-- * ...
--------------------------------------------------------------------------------

-- | ...
type M i     = ReaderT (Rim.Map (Linked i), Channels) (Program i)

-- | ...
type Order i = [Ordered i]

-- | ...
runM :: (HeaderCMD (IExp i) :<: i) => Channels -> Links i -> M i (IExp i a) -> Program i (Program i (IExp i a))
runM channels links = return . flip CMR.runReaderT (links, channels)

--------------------------------------------------------------------------------
-- ** Reading / Writing to and from Channels

-- | ...
read 
  :: forall proxy i a. (CompileExp (IExp i), Witness i a)
  => proxy i a -> Names (S Symbol i a) -> M i (E i a)
read _ n = go (witness :: Wit i a) n
  where
    go :: Wit i x -> Names (S Symbol i x) -> M i (E i x)
    go (WE)     (name) = asks $ varE . fst . lookupNode name . snd
    go (WP u v) (l, r) =
      do l' <- go u l
         r' <- go v r
         return (l', r')

-- | ...
write
  :: forall proxy i a. (SequentialCMD (IExp i) :<: i, Witness i a)
  => proxy i a -> Names (S Symbol i a) -> E i a -> M i ()
write _ n e = go (witness :: Wit i a) n e
  where
    go :: Wit i x -> Names (S Symbol i x) -> E i x -> M i ()
    go (WP u v) (l, r) (a, b) = go u l a >> go v r b
    go (WE)     (name) (expr) =
      do (c, k) <- asks $ lookupNode name . snd
         lift $ case k of
           E.Signal -> c <== expr
           _        -> c ==: expr

-- | ...
writed 
  :: forall proxy i a. (SequentialCMD (IExp i) :<: i, PredicateExp (IExp i) a, Typeable a)
  => proxy i (Identity a)
  -> Names (S Symbol i (Identity a))
  -> E i (Identity a)
  -> M i ()
writed _ name e =
  do d <- asks $ snd . lookupDelay name . snd
     lift $ d <== e

--------------------------------------------------------------------------------

type Si = State Int

fromLinks :: forall i a. CompileExp (IExp i) => Key i a -> Links i -> Channels
fromLinks (Key root) links =
  let m = fmap snd $ concat $ Rim.dump links
   in CMS.evalState (foldM add emptyChannels m) 0
  where
    add :: Channels -> Rim.HideType (Linked i) -> Si Channels
    add c (Rim.Hide (Linked (Delay {}) l@(Link out))) = newDelay l out c
    add c (Rim.Hide (Linked node       l@(Link out))) = 
      case isUseful node of
        Nothing -> return c
        Just k  -> newNode l out k c

    newNode :: forall x. Witness i x => Link i x -> Names (S Symbol i x) -> Kind -> Channels -> Si Channels
    newNode _ n k c = go (witness :: Wit i x) n c
      where
        go :: Wit i y -> Names (S Symbol i y) -> Channels -> Si Channels
        go (WP u v)   (l, r)    c = go u l c >> go v r c
        go (WE)  name@(Named n) c = do
          let kind = if root `eqStableName` n then E.Signal else k
          i <- next
          return $ insertNode name i kind c          

    newDelay :: forall x. Witness i x => Link i x -> Names (S Symbol i x) -> Channels -> Si Channels
    newDelay _ n c = go (witness :: Wit i x) n c
      where
        go :: Wit i y -> Names (S Symbol i y) -> Channels -> Si Channels
        go (WP u v) (l, r) c = go u l c >> go v r c
        go (WE)     (name) c = do
          old@(Ident d) <- next
          return $ insertDelay name old (Ident $ d ++ "_in") (insertNode name old E.Signal c)

    next :: Si Identifier
    next = do i <- CMS.get
              CMS.put (i + 1)
              return (Ident $ 'v' : show i)

--------------------------------------------------------------------------------

-- | Declare signal/variable instances for each node in 'order'
initialize
  :: forall i. ( SequentialCMD (IExp i) :<: i
               , ConcurrentCMD (IExp i) :<: i
               )
  => Channels
  -> Links i
  -> Order i
  -> Program i ()
initialize channels links order = forM_ order $ \(Ordered n) ->
  case Rim.lookup n links of
    Nothing -> error "Compiler.compile'_init: lookup failed"
    Just (Linked (Delay v _) (Link o)) -> initd o (Just v)
    Just (Linked (Repeat  v) (Link o)) -> init  o (Nothing)
    Just (Linked (Map   _ _) (Link o :: Link i x)) -> dist (witness :: Wit i x) o
      where
        dist :: Wit i a -> Names (S Symbol i a) -> Program i ()
        dist (WP u v) (l, r) = dist u l >> dist v r
        dist (WE)     (name) = init name (Nothing)
    _ -> return ()
  where
    init  :: forall a. Typeable a => Named (S Symbol i (Identity a)) -> Maybe (IExp i a) -> Program i ()
    init  n v =
      do let (i, k) = lookupNode n channels
         E.variableL i v

    initd :: forall a. Typeable a => Named (S Symbol i (Identity a)) -> Maybe (IExp i a) -> Program i ()
    initd n v =
      do let (i, o) = lookupDelay n channels
         E.signalG i v
         E.signalG o (Nothing :: Maybe (IExp i a))

--------------------------------------------------------------------------------
-- * Compilation
--------------------------------------------------------------------------------

comp' :: forall i. (SequentialCMD (IExp i) :<: i, CompileExp (IExp i)) => Ordered i -> M i ()
comp' (Ordered sym) =
  do (Linked n olink@(Link out)) <- asks $ (Rim.! sym) . fst
     case n of
       (Repeat c) ->
         do write olink out c
       (Map f ilink@(Link s)) ->
         do v <- read ilink s
            write olink out (f v)
       (Delay _ ilink@(Link s)) ->
         do e <- read ilink s
            writed olink out e
       _ -> return ()

compd' :: forall i. (SequentialCMD (IExp i) :<: i, CompileExp (IExp i)) => Ordered i -> M i ()
compd' (Ordered sym) =
  do (Linked (Delay _ (_ :: Link i (Identity b))) olink@(Link out)) <- asks $ (Rim.! sym) . fst
     (i, o) <- asks (lookupDelay out . snd)
     lift $ i <== (varE o :: IExp i b)

--------------------------------------------------------------------------------
-- **

compile'
  :: forall i a b.
     ( ConcurrentCMD (IExp i) :<: i
     , SequentialCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) a
     , PredicateExp  (IExp i) b
     , PredicateExp  (IExp i) Bool -- !!!
     , Typeable i                  -- !!!
     , Typeable a
     , Typeable b
     )
  => Key i (Identity b)
  -> Key i (Identity a)
  -> Links i
  -> [Ordered i]
  -> (Str i a -> Str i b)
compile' outp@(Key root) inp@(Key var) links order (Stream str) = Stream $ inArchitecture "test" $
  do -- initialization of inputs/outputs
     next <- str
     clk  <- clock
     signalPort (fst $ lookupNode (name var)  channels) In  (Nothing :: Maybe (IExp i a))
     signalPort (fst $ lookupNode (name root) channels) Out (Nothing :: Maybe (IExp i b))

     -- main loop
     runM channels links $ do
       val <- lift $ next
       
       -- network
       wrap "combinatorial" scomb $ do
         lift $ initialize channels links orders
         
         -- this is different from C, as we have drivers in VHDL
         --writeS inp (name var) val
         mapM_ comp' nodes
         mapM_ comp' delays

       wrapd "sequential" clk $ do
         mapM_ compd' delays
         
       -- output
       read outp (name root)
  where
    channels        = fromLinks outp links 
    (orders)        = foldr delete order [Ordered root, Ordered var]
    (delays, nodes) = filterDelays channels links order

    -- sensitivity lists for comb/seq processes    
    scomb :: [Identifier]
    scomb = fst (lookupNode (name var) channels) : (flip fmap delays $
              \(Ordered n) -> case (Rim.!) links n of
                Linked (Delay {}) (Link out) -> fst $ lookupNode out channels)

    -- wraps a 'M' action in a process
    wrap  :: String -> [Identifier] -> M i () -> M i ()
    wrap  s sl = lift . process (Ident s) sl . flip CMR.runReaderT (links, channels)

    -- wrapst a 'M' action in a process, only activates on a rising clock edge
    wrapd :: String -> Identifier -> M i () -> M i ()
    wrapd s sl@(Ident clk) = lift . process (Ident s) [sl] . E.when rising . flip CMR.runReaderT (links, channels)
      where
        rising :: IExp i Bool
        rising = varE (Ident $ "rising_edge(" ++ clk ++ ")")

    inArchitecture :: String -> Program i (Program i x) -> Program i (Program i x)
    inArchitecture name = return . architecture name . join

--------------------------------------------------------------------------------

compiler
  :: ( ConcurrentCMD (IExp i) :<: i
     , SequentialCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) a
     , PredicateExp  (IExp i) b
     , PredicateExp  (IExp i) Bool -- !!!
     , Typeable i                  -- !!!
     , Typeable a
     , Typeable b
     )
  => (Sig i a -> Sig i b) -> IO (Str i a -> Str i b)
compiler sf =
  do (root, input, nodes) <- freify sf     
     let order = sorter root  nodes
         cycle = cycles root  nodes
         links = linker order nodes     
     return $ case cycle of
       True  -> error "Compiler.compiler: found cycle"
       False -> compile' root input links order

--------------------------------------------------------------------------------
-- Some helpers
--------------------------------------------------------------------------------

-- | ...
filterDelays :: Channels -> Links i -> Order i -> (Order i, Order i)
filterDelays channels links order = partitionEithers $ flip fmap order $
  \o@(Ordered n) -> case Rim.lookup n links of
      Just (Linked (Delay _ _) _) -> P.Left  o
      Just _                      -> P.Right o

-- | ...
isUseful :: S sym i a -> Maybe Kind
isUseful (Join _ _)  = Nothing
isUseful (Left _)    = Nothing
isUseful (Right _)   = Nothing
isUseful (Var _)     = Just E.Signal
isUseful _           = Just E.Variable

--------------------------------------------------------------------------------
