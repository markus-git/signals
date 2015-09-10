{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler {-(compiler, compiler_fun)-} where

import Control.Monad.Operational.Compositional

import Signal.Core hiding (lift)
import Signal.Core.Stream 
import Signal.Core.Reify
import qualified Signal.Core        as S
import qualified Signal.Core.Stream as Str

import Signal.Compiler.Cycles
import Signal.Compiler.Linker
import Signal.Compiler.Sorter

import Language.VHDL (Identifier(..))
import Language.Embedded.VHDL hiding (name)
import qualified Language.Embedded.VHDL       as E
import qualified Language.Embedded.VHDL.Monad as EM

import Control.Arrow    (first, second)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State  (State, modify, gets, lift)
import Control.Monad.Identity
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State  as S
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
lookupDelay n c = case Map.lookup (hash n) (chan_delays c) of
  Nothing -> error $ "! " ++ show (Map.size (chan_delays c))
  Just x  -> x

  --(Map.!) (chan_delays c) (hash n)

insertNode  :: Named (S Symbol i (Identity a)) -> Identifier -> Kind -> Channels -> Channels
insertNode n i k (Channels ns ds) = Channels (Map.insert (hash n) (i, k) ns) ds

insertDelay :: Named (S Symbol i (Identity a)) -> Identifier -> Identifier -> Channels -> Channels
insertDelay n ni no (Channels ns ds) = Channels ns (Map.insert (hash n) (ni, no) ds)

markNode    :: Named (S Symbol i (Identity a)) -> Kind -> Channels -> Channels
markNode n k (Channels ns ds) = Channels (Map.adjust (second (const k)) (hash n) ns) ds

--------------------------------------------------------------------------------
-- ** Initialization

type Init = State (Integer, Channels)

makeChannels :: forall i a. CompileExp (IExp i) => Key i a -> Links i -> Channels
makeChannels (Key root) m = let links = fmap snd . concat $ Rim.dump m in
    snd . flip S.execState (1, emptyChannels) $ mapM_ add links
  where
    add :: Rim.HideType (Linked i) -> Init ()
    add (Rim.Hide (Linked (Delay _ _) l@(Link out))) = {-newNode l out (M.Signal) >> -}newDelay l out
    add (Rim.Hide (Linked s           l@(Link out))) =
      case isUseful s of
        Nothing -> return ()
        Just k  -> newNode l out k

    newDelay :: forall x. Witness i x => Link i x -> Names (S Symbol i x) -> Init ()
    newDelay _ n = go (wit :: Wit i x) n
      where
        go :: Wit i y -> Names (S Symbol i y) -> Init ()
        go (WP u v) (l, r) = go u l >> go v r
        go (WE)     (name) =
          do old@(Ident d) <- next
             let new = Ident (d ++ "_in")
             modify $ second $ insertNode  name old E.Signal
             modify $ second $ insertDelay name old new

    newNode  :: forall x. Witness i x => Link i x -> Names (S Symbol i x) -> Kind -> Init ()
    newNode _ n k = go (wit :: Wit i x) n
      where
        go :: Wit i y -> Names (S Symbol i y) -> Init ()
        go (WP u v)  (l, r)    = go u l >> go v r
        go (WE) name@(Named n) =
          -- ! this is a bit clunky
          do let  kind = if root `eqStableName` n then E.Signal else k
             i <- next
             modify $ second $ insertNode name i kind
    
    next :: Init Identifier
    next = do
      i <- gets fst
      modify $ first (+ 1)
      return $ newVar i

isUseful :: S sym i a -> Maybe Kind
isUseful (Join _ _)  = Nothing
isUseful (Left _)    = Nothing
isUseful (Right _)   = Nothing
isUseful (Var _)     = Just E.Signal
isUseful _           = Just E.Variable

--------------------------------------------------------------------------------
-- ** Reading / Writing to and from Channels

type M i = ReaderT (Rim.Map (Linked i), Channels) (Program i)

read 
  :: forall proxy i a. (CompileExp (IExp i), Witness i a)
  => proxy i a -> Names (S Symbol i a) -> M i (U i a)
read _ n = go (wit :: Wit i a) n
  where
    go :: Wit i x -> Names (S Symbol i x) -> M i (U i x)
    go (WE)     (name) = asks $ varE . fst . lookupNode name . snd
    go (WP u v) (l, r) =
      do l' <- go u l
         r' <- go v r
         return (l', r')

write
  :: forall proxy i a. (SequentialCMD (IExp i) :<: i, Witness i a)
  => proxy i a -> Names (S Symbol i a) -> U i a -> M i ()
write _ n e = go (wit :: Wit i a) n e
  where
    go :: Wit i x -> Names (S Symbol i x) -> U i x -> M i ()
    go (WP u v) (l, r) (a, b) = go u l a >> go v r b
    go (WE)     (name) (expr) =
      do (c, k) <- asks $ lookupNode name . snd
         lift $ case k of
           E.Signal -> c <== expr
           _        -> c ==: expr

--------------------------------------------------------------------------------

writed
  :: forall proxy i a. (SequentialCMD (IExp i) :<: i, PredicateExp (IExp i) a, Typeable a)
  => proxy i (Identity a)
  -> Names (S Symbol i (Identity a))
  -> U i (Identity a)
  -> M i ()
writed _ name e =
  do d <- asks $ snd . lookupDelay name . snd
     lift $ d <== e

--------------------------------------------------------------------------------
-- * Compilation
--------------------------------------------------------------------------------

comp' :: forall i. (SequentialCMD (IExp i) :<: i, CompileExp (IExp i)) => Ordered i -> M i ()
comp' (Ordered sym) =
  do (Linked n olink@(Link out)) <- asks $ (Rim.! sym) . fst
     case n of
       (Repeat c) ->
         do v <- down c
            write olink out v
       (Map f ilink@(Link s)) ->
         do e <- read ilink s
            v <- up   ilink e
            let o = f v
            y <- down o
            write olink out y            
       (Delay _ ilink@(Link s)) ->
         do e <- read ilink s
            writed olink out e
       _ -> return ()
  where
    down :: Stream i b -> M i b
    down = lift . Str.run

    up   :: proxy i b -> U i b -> M i (Stream i (U i b))
    up _ = return . Str.Stream . return . return 

--------------------------------------------------------------------------------

compd' :: forall i. (SequentialCMD (IExp i) :<: i, CompileExp (IExp i)) => Ordered i -> M i ()
compd' (Ordered sym) =
  do channels <- asks snd
     (Linked (Delay _ (_ :: Link i (Identity b))) olink@(Link out)) <- asks $ (Rim.! sym) . fst
     let (i, o) = lookupDelay out channels
     lift $ i <== (varE o :: IExp i b)

--------------------------------------------------------------------------------
-- **

type Order i = [Ordered i]

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
    Just (Linked (Map   _ _) (Link o :: Link i x)) -> dist (wit :: Wit i x) o
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
-- **

compile'_fun
  :: forall i a b.
     ( ConcurrentCMD (IExp i) :<: i
     , SequentialCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) a
     , PredicateExp  (IExp i) b
     , Typeable i
     , Typeable a
     , Typeable b
     )
  => Key i (Identity b)
  -> Key i (Identity a)
  -> Links i
  -> [Ordered i]
  -> (Str i a -> Str i b)
compile'_fun outp@(Key root) inp@(Key var) links order (Stream str) = Stream $ 
  do next <- str
     clk  <- clock
     signal (fst $ lookupNode (name var)  channels) In  (Nothing :: Maybe (IExp i a))
     signal (fst $ lookupNode (name root) channels) Out (Nothing :: Maybe (IExp i b))
     compile'_run  channels links $ do
       -- input
       val <- lift $ next
       
       -- network
       wrap "combinatorial" scomb $ do
         lift $ initialize channels links orders
         -- this is different from C, as we have drivers in VHDL
         --writeS inp (name var) val
         mapM_ comp' nodes
         mapM_ comp' delays

       wrap "sequential" [clk] $ do
         mapM_ compd' delays
         
       -- output
       read outp (name root)
  where
    channels        = makeChannels outp links 
    (orders)        = foldr delete order [Ordered root, Ordered var]
    (delays, nodes) = filterDelays channels links order

    -- sensitivity lists for comb/seq processes    
    scomb :: [Identifier]
    scomb = fst (lookupNode (name var) channels) : (flip fmap delays $
              \(Ordered n) -> case (Rim.!) links n of
                Linked (Delay {}) (Link out) -> fst $ lookupNode out channels)

    -- Wraps a 'M' action in a process
    wrap :: String -> [Identifier] -> M i () -> M i ()
    wrap s sl = lift . process (Ident s) sl . flip R.runReaderT (links, channels)

-- | ...
filterDelays :: Channels -> Links i -> Order i -> (Order i, Order i)
filterDelays channels links order = partitionEithers $ flip fmap order $
  \o@(Ordered n) -> case Rim.lookup n links of
      Just (Linked (Delay _ _) _) -> P.Left  o
      Just _                      -> P.Right o

-- | ...
compile'_run
  :: Channels
  -> Links i
  -> M i (IExp i a)
  -> Program i (Program i (IExp i a))
compile'_run channels links = return . flip R.runReaderT (links, channels)

--------------------------------------------------------------------------------

compiler_fun
  :: ( ConcurrentCMD (IExp i) :<: i
     , SequentialCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) a
     , PredicateExp  (IExp i) b
     , Typeable i
     , Typeable a
     , Typeable b
     )
  => (Sig i a -> Sig i b) -> IO (Str i a -> Str i b)
compiler_fun sf =
  do (root, input, nodes) <- reify_fun sf     
     let order = sorter root  nodes
         cycle = cycles root  nodes
         links = linker order nodes     
     return $ case cycle of
       True  -> error "Compiler.compiler: found cycle"
       False -> compile'_fun root input links order

--------------------------------------------------------------------------------
-- **
{-
compile'
  :: forall i a.
     ( ConcurrentCMD (IExp i) :<: i
     , SequentialCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) a
     , Typeable a
     )
  => Key i (Identity a)
  -> Links i
  -> [Ordered i]
  -> Str i a
compile' outp@(Key root) links order = Stream $ 
  do signal (fst $ lookupNode (name root) channels) Out (Nothing :: Maybe (IExp i a))
     compile'_init channels links orders
     compile'_run  channels links $ do
       -- network
       wrap $ do
         mapM_ comp' nodes
         mapM_ comp' delays
       -- output
       read outp (name root)
  where
    channels        = makeChannels outp links    
    (orders)        = foldr delete order [Ordered root]
    (delays, nodes) = compile'_split channels links order

    -- Wraps a 'M' action in a process
    wrap :: M i () -> M i ()
    wrap = lift . process (Ident "main") [] . flip R.runReaderT (links, channels)
-}
--------------------------------------------------------------------------------
{-
compiler
  :: ( ConcurrentCMD (IExp i) :<: i
     , SequentialCMD (IExp i) :<: i
     , HeaderCMD     (IExp i) :<: i
     , CompileExp    (IExp i)
     , PredicateExp  (IExp i) a
     , Typeable a
     )
  => Sig i a -> IO (Str i a)
compiler sig =
  do (root, nodes) <- reify sig

     let order = sorter root  nodes
         cycle = cycles root  nodes
         links = linker order nodes
     
     return $ case cycle of
       True  -> error "Compiler.compiler: found cycle"
       False -> compile' root links order
-}
--------------------------------------------------------------------------------
