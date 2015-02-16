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

import Frontend.Knot

import           Frontend.Stream (Stream(..), Str)
import qualified Frontend.Stream as Str

import           Frontend.Signal ( Signal, Sig
                                 , Empty(..)
                                 , Struct(..)
                                 , TStruct(..), tleaf, tleft, tright, rep)
import qualified Frontend.Signal as Sig

import           Frontend.SignalObsv (TSignal(..), showTS, showP, edges)
import qualified Frontend.SignalObsv as SigO

import           Control.Monad.Writer (WriterT, MonadWriter)
import qualified Control.Monad.Writer as CMW

import           Control.Monad.Reader (ReaderT, MonadReader, MonadTrans, lift)
import qualified Control.Monad.Reader as CMR

import           Control.Monad.State (StateT, State, MonadState, get, put, modify)
import qualified Control.Monad.State as CMS

import           Control.Monad.Identity (Identity, runIdentity)
import qualified Control.Monad.Identity as CMI

import           Data.Map (Map, (!))
import qualified Data.Map as M

import           Core (CMD)
import qualified Core as C

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Operational

import Data.List (find, sortBy, elem, mapAccumR)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.Dynamic
import Data.Reify
import Data.Proxy
import Data.Typeable

import           Prelude hiding (cycle)
import qualified Prelude as P

---------------------------------------- : Testing
import qualified Expr as E
import qualified Backend.C as B
import Text.PrettyPrint.Mainland
---------------------------------------- : End

--------------------------------------------------------------------------------
-- * Compiler
--------------------------------------------------------------------------------

-- ToDo: result is in 'e' since we use references for everything

compile :: (Typeable e, Typeable a, Typeable b, Num (e Int), Integral (e Int))
        => (Sig e a -> Sig e b)
        -> IO (Program (CMD e) (e a) -> Program (CMD e) (e b))
compile f =
  do (Graph ns root) <- reifyGraph f
     let nodes = M.fromList ns
         links = linker nodes
         order = sorter root nodes
     
     return $ \input ->
       do (new, buffers) <- optimize nodes
          compiler new links order buffers input

optimize :: forall e a. (Typeable e, Typeable a, Num (e Int), Integral (e Int))
  => Map Unique (Node e)
  -> Program (CMD e)
       ( Map Unique (Node e)
       , Map Unique (Buffer e a))
optimize nodes =
  do let (new, vals) = opt_delay_chains nodes
         (ids, exps) = unzip $ M.toList vals
     buffer <- sequence $ map (\es -> newBuff (fromIntegral $ length es) (head es)) exps
     return $ (new, M.fromList $ zip ids buffer)

--------------------------------------------------------------------------------

type Node e = TSignal e Unique

--------------------------------------------------------------------------------
-- * Linker
--------------------------------------------------------------------------------

type Id     = String

-- | ...
linker :: Map Unique (Node e) -> Map Id Id
linker nodes =
        snd
      . runIdentity
      . tie
      . sequence
      . fmap (link . showP)
      $ M.toList nodes

-- | ...
link :: (Id, TSignal exp Id) -> Knot Id Id ()
link (i, TLambda l r) =
  do l' <- knot l
     r' <- knot r
     i =: (l' ++ ". " ++ r')

link (i, TVar) =
  do i =: i

link (i, TConst _) =
  do i =: i

link (i, TLift _ s) =
  do knot s
     i =: i

link (i, TMap t t' _ s) =
  do s' <- asks t s
     tells t  s' i
     i =: i -- not ok for mult. outputs

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
     i =: s'

--------------------------------------------------------------------------------

asks :: TStruct exp a -> String -> Knot Id Id (TStruct exp a)
asks (TLeaf _)   s = knot s >>= return . TLeaf
asks (TPair l r) s =
  do l' <- asks l $ s ++ "_1"
     r' <- asks r $ s ++ "_2"
     return $ TPair l' r'

tells :: TStruct exp a -> TStruct exp a -> Id -> Knot Id Id ()
tells (TLeaf _)   t s = s =: (tleaf t)
tells (TPair l r) t s =
  do tells l (tleft  t) (s ++ "_1")
     tells r (tright t) (s ++ "_2")

--------------------------------------------------------------------------------
-- * Sorter
--------------------------------------------------------------------------------

data Status  = Visited | Visiting | Unvisited
type Order   = Int
type Pred    = Unique

sorter :: Unique
       -> Map Unique (Node e)
       -> Map Unique Order
sorter root nodes =
  case cycles initC root of
    False -> M.map (\(_,o,_) -> o) $ snd $ CMS.execState (sort root) initS
  where
    initC =     M.map ((,,) Unvisited 0) nodes
    initS = (1, M.map ((,,) Unvisited 0) nodes) 
    
    cycles s i =
      let m = CMS.execState (cycle i) s
          n = M.filter (\(s,_,_) -> isUnvisited s) m
      in case M.null n of
           True  -> False
           False -> cycles m (fst $ M.findMin n)

cycle :: Unique -> State (Map Unique (Status, Pred, Node e)) ()
cycle i =
  do mark i Visiting
     us <- adjacent i
     forM_ us $ \u ->
       do p <- predecessor i
          s <- status u
          when (isVisiting  s && p /= u) $ error $ "found cycle from " ++ show i ++ " to " ++ show u
          when (isUnvisited s)           $ pred u i >> cycle u
     mark i Visited
  where
    mark :: Unique -> Status -> State (Map Unique (Status, Pred, Node e)) ()
    mark i s = modify $ flip M.adjust i $ \(_, p, n) -> (s, p, n)

    pred :: Unique -> Pred -> State (Map Unique (Status, Pred, Node e)) ()
    pred i p = modify $ flip M.adjust i $ \(s, _, n) -> (s, p, n)

    status :: Unique -> State (Map Unique (Status, Pred, Node e)) Status
    status i = get >>= return . (\(s, _, _) -> s) . (! i)

    adjacent :: Unique -> State (Map Unique (Status, Pred, Node e)) [Unique]
    adjacent i = get >>= return . (\(_, _, n) -> edges' n) . (! i)
      where
        edges' (TDelay {}) = []
        edges' x           = edges x
    
    predecessor :: Unique -> State (Map Unique (Status, Pred, Node e)) Pred
    predecessor i = get >>= return . (\(_, p, _) -> p) . (! i)

sort :: Unique -> State (Int, Map Unique (Status, Order, Node e)) ()
sort i =
  do mark i Visited
     us <- adjacent i
     forM_ us $ \u ->
       do s <- status u
          when (isUnvisited s) (sort u)
     o  <- new
     tag i o
  where
    new :: State (Int, Map Unique (Status, Order, Node e)) Order
    new = do (i, m) <- get
             put (i + 1, m)
             return i

    tag :: Unique -> Order -> State (Int, Map Unique (Status, Order, Node e)) ()
    tag i o = modify $ second $ flip M.adjust i $ \(s, _, n) -> (s, o, n)

    mark :: Unique -> Status -> State (Int, Map Unique (Status, Order, Node e)) ()
    mark i s = modify $ second $ flip M.adjust i $ \(_, o, n) -> (s, o, n)

    status :: Unique -> State (Int, Map Unique (Status, Order, Node e)) Status
    status i = get >>= return . (\(s,_,_) -> s) . (! i) . snd
    
    adjacent :: Unique -> State (Int, Map Unique (Status, Order, Node e)) [Unique]
    adjacent i = get >>= return . edges . (\(_,_,n) -> n) . (! i) . snd

--------------------------------------------------------------------------------

isVisiting :: Status -> Bool
isVisiting (Visiting) = True
isVisiting _          = False

isUnvisited :: Status -> Bool
isUnvisited (Unvisited) = True
isUnvisited _           = False

--------------------------------------------------------------------------------
-- * Compiler
--------------------------------------------------------------------------------

-- | ...
type Prog e = ReaderT (Map Id Id) (StateT (Map Id Dynamic) (Program (CMD e)))

compiler :: forall exp a b. (Typeable exp, Typeable b, Typeable a, Num (exp Int))
         => Map Unique (Node exp) -- nodes in graph
         -> Map Id Id             -- links
         -> Map Unique Order      -- order
         -> Map Unique (Buffer exp a)
         -> Program (CMD exp) (exp b)
         -> Program (CMD exp) (exp a)
compiler nodes links order buffers input =
  do m <- run $ mapM_ (\n -> comp n buffers input) o
     case fromDynamic (m ! (show $ fst $ last o)) of
       Just r  -> C.getRef r
       Nothing -> error "compiler"
  where
    l = filterMap nodes (read . (:[]) . head) isNOP links
    o = sort $ filterMap nodes id isNOP order
    
    run :: Prog exp () -> Program (CMD exp) (Map Id Dynamic)
    run = flip CMS.execStateT M.empty
        . flip CMR.runReaderT l

    sort :: Map Unique Order -> [(Unique, Node exp)]
    sort = fmap   ((\i -> (i, nodes ! i)) .  fst)
         . sortBy (compare `on` snd)
         . M.toList

comp :: (Typeable exp, Typeable b, Typeable a, Num (exp Int))
     => (Unique, Node exp)
     -> Map Unique (Buffer exp a)
     -> Program (CMD exp) (exp b)
     -> Prog exp ()
comp (i, TVar) _ input =
  do v <- lift $ lift $ liftProgram input -- I'll be buff in no time with
     r <- lift $ lift $ C.newRef v        -- all this lifting
     setLink (show i) r

comp (i, TConst c) _ _ =
  do v <- lift $ lift $ liftProgram $ Str.run c -- Meh..
     r <- lift $ lift $ C.newRef v
     setLink (show i) r

comp (i, TLift f s) _ _ =
  do r <- getLink (show s)
     let str = Str.Stream $ return $ C.getRef r
     v <- lift $ lift $ liftProgram $ Str.run $ f str
     k <- lift $ lift $ C.newRef v
     setLink (show i) k

comp (i, TMap t t' f s) _ _ =
  do v <- getLinks t (show i)
     setLinks (f v) (show i)

comp (i, TDBuff s j) buffers _ =
  do let b = buffers !? s
     v <- lift $ lift $ getBuff b (j - 1)
     r <- lift $ lift $ C.newRef v
     setLink (show i) r

comp (i, TVBuff s) buffers _ =
  do let b = buffers !? i
     r <- getLink (show s)
     v <- lift $ lift $ C.getRef r
     lift $ lift $ putBuff b v
     setLink (show i) r

--comp (i, n) = error $ "Missing comp. for: " ++ show n

(!?) :: Ord i => Map i x -> i -> x
m !? i = case M.lookup i m of
           Just x  -> x
           Nothing -> error $ "!"

--------------------------------------------------------------------------------

getLink :: Typeable a => Id -> Prog exp (C.Ref a)
getLink u =
  do x <- CMR.asks (!? u)
     y <- CMS.gets (!? x)
     case fromDynamic y of
       Just r  -> return r
       Nothing -> error "getLink"

setLink :: Typeable a => Id -> C.Ref a -> Prog exp ()
setLink u r =
  do x <- CMR.asks (! u)
     CMS.modify (M.insert x (toDyn r))

--------------------------------------------------------------------------------

getLinks :: Typeable exp => TStruct exp a -> Id -> Prog exp (Struct exp a)
getLinks (TLeaf _) s =
  do r <- getLink s
     v <- lift $ lift $ C.unsafeGetRef r
     return $ Leaf v
getLinks (TPair l r) s =
  do sl <- getLinks l $ s ++ "_1"
     sr <- getLinks r $ s ++ "_2"
     return (Pair sl sr)

setLinks :: Typeable exp => Struct exp a -> Id -> Prog exp ()
setLinks (Leaf e) s = lift (lift (C.newRef e)) >>= setLink s

-- we would need a proper 'setLinks' function, but first we need to deal with
-- linking case where 'TMap' returnes a struct of values

--------------------------------------------------------------------------------

filterMap :: Ord i => Map i x -> (j -> i) -> (x -> Bool) -> Map j y -> Map j y
filterMap m f g = M.filterWithKey (\k _ -> not . g . (m !) $ f k)

isNOP :: TSignal e a -> Bool
isNOP (TZip    {}) = True
isNOP (TFst    {}) = True
isNOP (TSnd    {}) = True
isNOP (TLambda {}) = True
isNOP _            = False

isDelay :: TSignal e a -> Bool
isDelay (TDelay {}) = True
isDelay _           = False

--------------------------------------------------------------------------------
-- * Optimizing
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Buffers

data Buffer exp a = Buffer
  { getBuff :: exp Int -> Program (CMD exp) (exp a)
  , putBuff :: exp a   -> Program (CMD exp) ()
  }

newBuff
  :: forall exp a. (Num (exp Int), Integral (exp Int))
  => exp Int -> exp a -> Program (CMD exp) (Buffer exp a)
newBuff size init =
  do arr <- C.newArr size init
     ir  <- C.newRef (0 :: exp Int)
     let get j = do i <- C.unsafeGetRef ir
                    C.getArr ((i + (size - j - 1)) `mod` size) arr
     let put a = do i <- C.unsafeGetRef ir
                    C.setArr i a arr
                    -- missing iff
     return $ Buffer get put

--------------------------------------------------------------------------------

opt_delay_chains :: (Typeable e, Typeable a, Num (e Int))
  => Map Unique (Node e)
  -> ( Map Unique (Node e)
     , Map Unique [e a])
opt_delay_chains nodes =
  let x = find_chains nodes
      (y, buffs) = buffer_chains x
      z = merge_chains nodes y
  in  (z, buffs)

find_chains :: Map Unique (Node e) -> Map Unique [(Unique, Node e)]
find_chains nodes =
  let delays = M.foldrWithKey (\k n -> M.insert (edge n) (k, n)) M.empty
             $ M.filter isDelay nodes
      heads  = M.foldr (M.delete . fst) delays delays
  in  M.map (flip chain delays) heads
  where
    chain :: (Unique, Node e) -> Map Unique (Unique, Node e) -> [(Unique, Node e)] 
    chain v@(i, _) m = v : maybe [] (flip chain m) (M.lookup i m)

    edge :: Node e -> Unique
    edge = head . edges
  
buffer_chains
  :: forall e a. (Typeable e, Typeable a, Num (e Int))
  => Map Unique [(Unique, Node e)]
  -> ( Map Unique [(Unique, Node e)]
     , Map Unique [e a])
buffer_chains chains =
  let vals  = M.map (map val) chains
      buffs = M.mapWithKey (\k -> snd . mapAccumR (acc k) 1) chains
  in (buffs, vals)
  where
    val     (i, TDelay v _) = fromJust $ cast v
    acc k n (i, TDelay v _) = (n+1, (i, TDBuff k $ fromIntegral n))

merge_chains
  :: Map Unique (Node e)
  -> Map Unique [(Unique, Node e)]
  -> Map Unique (Node e)
merge_chains nodes chains =
  let nodes' = M.foldr (flip $ foldr (\(u,n) -> M.adjust (const n) u)) nodes chains
      heads' = M.foldrWithKey (\k _ -> M.insert (-k) (TVBuff k)) M.empty chains
  in  M.union nodes' heads'

--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

type E  = E.Expr Float
type S  = Sig E.Expr Float

sig :: S -> S
sig s = s + 0

tex :: IO (Program (CMD E.Expr) ())
tex = do
  prog  <- compile $ fir [1,2,3]
  return $ do
    inp <- C.open "input"
    out <- C.open "output"
    let getty = prog (C.fget inp)
        setty = C.fput out
    C.while (return $ E.tru)
            (getty >>= setty)

test :: IO Doc
test = do
  p <- tex
  B.cgen $ C.mkFunction "main" p

--------------------------------------------------------------------------------

sums :: [S] -> S
sums = foldr1 (+)

muls :: [E] -> [S] -> [S]
muls es = zipWith (*) (map Sig.repeat es)

delays :: [E] -> S -> [S]
delays = flip $ scanl (flip Sig.delay)

fir :: [E] -> S -> S
fir as = sums . muls as . delays ds
  where ds = replicate (length as - 1) 0

iir :: [E] -> [E] -> S -> S
iir (x:a:as) bs s = o
  where
    u = fir bs s
    l = fir as $ Sig.delay a o
    o = (1 / Sig.repeat x) * (u - l)

testShow :: IO ()
testShow = do
  putStrLn "========== Graph: "
  g@(Graph nodes root) <- reifyGraph $ fir [1,2,3]
  let m = M.fromList nodes
  putStrLn $ show g
  putStrLn "=========== Links: "
  let l = linker m
  putStrLn $ show l
--putStrLn "----- Filtered: "
--putStrLn $ show $ filterMap m (read . (:[]) . head) isNOP l
  putStrLn "=========== Order: "
  let o = sorter root m
  putStrLn $ show o
--putStrLn "----- Filtered: "
--putStrLn $ show $ filterMap m id isNOP o
  putStrLn "=========== Chains: "
  let cs     = find_chains m
      (b, v) = buffer_chains cs :: (Map Unique [(Unique, Node E.Expr)], Map Unique [E])
      gs     = merge_chains m b
  putStrLn $ show cs
  putStrLn "........... "
  putStrLn $ show b
  putStrLn "........... "
  putStrLn $ show gs
  putStrLn "=========== "
