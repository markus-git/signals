{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Frontend.SignalComp where

import           Frontend.Stream (Stream(..), Str)
import qualified Frontend.Stream as Str

import           Frontend.Signal (Signal, Sig, Struct(..), Empty(..))
import qualified Frontend.Signal as Sig

import           Frontend.SignalObsv (TSignal(..))
import qualified Frontend.SignalObsv as SigO

import           Data.Map (Map)
import qualified Data.Map as M

import Expr                (Expr(..), eq)
import Interpretation      (VarPred)
import Core                (CMD, Ref(..), newRef, setRef, getRef
                               , Arr(..), newArr, setArr, getArr
                               , iff)

import Control.Applicative
import Control.Monad.Operational (Program)

import           Control.Monad.Writer (WriterT, MonadWriter)
import qualified Control.Monad.Writer as CMW

import           Control.Monad.Reader (ReaderT, MonadReader, MonadTrans, lift)
import qualified Control.Monad.Reader as CMR

import           Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity as CMI

import Data.Dynamic
import Data.Reify
import Data.Proxy
import Data.Typeable

import           Prelude
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Compiler
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Knot Monad

newtype KnotT i x m a =
    KnotT { unKnotT :: ReaderT (Map i x) (WriterT [(i, x)] m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Map i x)
    , MonadWriter [(i, x)]
    )

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

--------------------------------------------------------------------------------
-- ** Graph

type SGraph exp   = Graph (TSignal exp)

type SNode  exp   = (Unique, TSignal exp Unique)

type SProg  exp a = Program (CMD exp) (exp a)

compiler :: (Typeable exp, Typeable a, Typeable b)
         => SGraph exp   -- ^ signal graph
         -> SProg  exp a -- ^ input  program
         -> SProg  exp b -- ^ output program
compiler (Graph nodes root) input = undefined

----------------------------------------
-- ...

type Knot  exp a = KnotT String Dynamic (Program (CMD exp)) a

compile :: forall exp a. (Typeable exp, Typeable a)
        => SNode exp     -- ^ signal node
        -> SProg exp a   -- ^ input  program : remove later
        -> Knot  exp ()  -- ^ output knot

compile (i, TLambda l r) _ =
  do sl <- knot $ show l
     sr <- knot $ show r -- I'm not sure if this is correct,
     show i =: sr        -- or if 'sl <- knot l' is unecessary

compile (i, TVar) input =
  do s <- lift $ input
     r <- tangle s       -- : lift $ toDyn <$> newRef
     show i =: r

compile (i, TConst c) _ =
  do s <- lift $ Str.run c
     r <- tangle s
     show i =: r

compile (i, TLift f c) _ =
  do s <- knot $ show c
     t <- untangle s
     g <- lift $ streamify f t -- : lift $ Stream . f . unStream $ t
     r <- tangle g
     show i =: r

{-
  do s <- knot $ show c
     t <- untangleS s
     g <- return $ f t
     r <- tangleS g
     show i =: r
-}

--------------------------------------------------------------------------------
-- Some helper functions

-- | does some unwrapping/wrapping of stream functions
streamify   :: (Str exp a -> Str exp b) -> exp a -> SProg exp b
streamify f = Str.run . f . Stream . return . return

--------------------------------------------------------------------------------
-- Some dynamic helper functions
--
-- ToDo: we should remove 'Dynamic' later on, so I put all the
--       functions which invlove typecasting here.

-- | casts a reference to a dynamic type
tangle      :: (Typeable exp, Typeable a) => exp a -> Knot exp Dynamic
tangle e    = lift $ toDyn <$> newRef e

-- | fetches the value from a dynamic reference
untangle    :: (Typeable exp, Typeable a) => Dynamic -> Knot exp (exp a)
untangle d  = lift $ getRef $ unDyn d

-- | same as tangle, but for structs
tangleS     :: (Typeable a) => Struct a -> Knot exp Dynamic
tangleS s   = return $ toDyn s

-- | same as untangle, but for structs
untangleS   :: (Typeable exp, Typeable a) => Dynamic -> Knot exp (Struct a)
untangleS d = return $ unDyn d

-- | removes the dynamic type from a reference
unDyn       :: Typeable a => Dynamic -> a
unDyn d     = case fromDynamic d of
                Just e  -> e
                Nothing -> error "unDyn"

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

{-

type Pro a = Program (CMD Expr) (Expr a)
type Str a = Stream             (Expr a)

compile :: forall a b. (Typeable a, Typeable b)
          =>    (Sig a -> Sig b)
          -> IO (Pro a -> Str b)
compile f = do
  g <- reifyGraph f
  return $ \i -> Stream $ do
    (b, g') <- delayChains g
--    error $ show g'
    return $ compileGraph g' b i

-}

--------------------------------------------------------------------------------
-- ** Helper types / functions

{-

type Prg a = Program (CMD Expr) a
type Node  = (Unique, TSignal Unique)
type Chain = [Node]
type Id    = Unique

findNode :: [Node] -> Id -> Maybe Node
findNode nodes i = find ((==) i . P.fst) nodes

-}

--------------------------------------------------------------------------------
-- Nothing to see here citizen, move along.

{-

type Maps = Map Id Dynamic

-- ToDo: Change how references are handeled
-- ToDo: Reader, Writer compiler
-- ToDo: Some other restriction than "Any" on Expr will lead to problems..

compileGraph :: forall a b . (Typeable a, Typeable b)
             => Graph TSignal
             -> Map Unique (Buffer a)
             -> Pro a
             -> Pro b
compileGraph (Graph nodes root) buffers input = do
    (d, m') <- compileNode (fromJust $ findNode nodes root) M.empty

    mapM (\n -> compileNode n m' >>= \(p, _) -> return p) (filter ((<0) . fst) nodes)

    let x :: Maybe (Ref (Expr b))
        x = fromDynamic d

        y :: Maybe (Struct (Expr b))
        y = fromDynamic d

    case (x, y) of
      (Just r, _)        -> getRef r
      (_, Just (Leaf s)) -> return s
  where
      -- ToDo: I really need to have a linker stage, this is a hack to get
      --       things running...
    compileNode :: Node -> Map Id Dynamic -> Prg (Dynamic, Map Id Dynamic)
    compileNode (i, n) m
      | Just d <- M.lookup i m = return (d, m)
      | otherwise              = case n of
          (TVar) -> do
            v <- input
            r <- newRef' v
            return (toDyn r, add i (toDyn r) m)

            -- If I make "Proxy t" into "Proxy (Expr t)" then functions
            -- will be limited, more so than for TConst and TLift
          (TLambda (_ :: Proxy t) s u) -> do
            (s', m')  <- load s m
            (u', m'') <- load u m'

            -- the proxy type "t" here represents the desired return value
            -- of the function. Which will either be a reference to such a
            -- value, or a struct of the value.
            let x :: Maybe (Ref t)
                x = fromDynamic u'

                y :: Maybe (Struct t)
                y = fromDynamic u'

            case (x, y) of
              (Just r, _) -> return (toDyn r, m'')
              (_, Just r) -> return (toDyn r, m'')


            -- I'm hoping no one will reference a const node...
          (TConst s) -> do
            s' <- Str.runStream s
            r  <- newRef s'
            return (toDyn r, m)
          (TLift (f :: Stream (Expr t0) -> Stream (Expr t1)) s) -> do
            r        <- initRef                     :: Prg (Ref (Expr t1))
            (s', m') <- load' s (add i (toDyn r) m) :: Prg (Ref (Expr t0), Maps)
            v        <- Str.runStream $ f $ Str.Stream $ return $ getRef s'
            setRef r v
            return (toDyn r, m')


          (TMap (f :: Struct t0 -> Struct t1) s) -> do
            (Ex r, m') <- compileStruct (find s) m
            s'         <- r2s r
            -- hm... do I still need the convice it that s' is typeable?
            case ws s' of
              Wt -> do let x   = f $ fromJust $ cast s'
                           m'' = add i (toDyn x) m'
                       return (toDyn x, m'')


          (TVBuff s) -> do
            let buff = fromJust  $ M.lookup i buffers
            let n'   = case M.lookup s m of
                         Just x  -> x
                         Nothing -> error $ "\ncouldn't find node " ++ show s ++
                                             "\nin map \n" ++ show m ++
                                             "\nor in graph \n" ++ show nodes

            let x :: Maybe (Ref (Expr a))
                x = fromDynamic n'

            let y :: Maybe (Struct (Expr a))
                y = fromDynamic n'

            case (x, y) of
              (Just r, _) -> do v <- getRef r
                                putBuff buff v
                                r <- newRef' v
                                return (toDyn r, m)
              (_, Just s) -> do let v = case s of (Leaf v) -> v
                                putBuff buff v
                                r <- newRef' v
                                return (toDyn r, m)

          (TDBuff s i) -> do
            let buff = fromJust $ M.lookup s buffers
            v <- getBuff buff (i-1) :: Program (CMD Expr) (Expr a)
            r <- newRef' v
            return (toDyn r, m)


          (TDelay (d :: Expr t0) s) -> do
            r <- initRef :: Prg (Ref (Expr t0))
            p <- initRef -- values from previous iteration
            o <- initRef -- current output value
            (vr, m') <- load' s (add i (toDyn r) m)
            next     <- getRef vr

              -- if we could init global vars with a value we wouldn't
              -- need this if statement
            iff (litExp $ True)
                (do setRef p next
                    setRef o d)
                (do prev <- getRef p
                    setRef o prev
                    setRef p next)

            return (toDyn o, m)

    compileStruct :: Node -> Map Id Dynamic -> Prg (Ex, Map Id Dynamic)
    compileStruct (i, n) m = case n of
          (TZip l r) -> do
            (Ex l', m')  <- compileStruct (find l) m
            (Ex r', m'') <- compileStruct (find r) m'
            return (Ex (Pair' l' r'), m'')
          (TFst l) -> do
            (Ex l', m') <- compileStruct (find l) m
            case l' of
              Pair' x _ -> return (Ex x, m')
          (TSnd r) -> do
            (Ex r', m') <- compileStruct (find r) m
            case r' of
              Pair' _ x -> return (Ex x, m')
          (TMap (f :: Struct t0 -> Struct t1) s) -> do
            (n', m') <- load' i m :: Prg (Struct t1, Maps)
            r'       <- s2r n'
            return (Ex r', m')
          _  -> do
              -- I can't remember why I put Float here.. probably Typeable
            (n', m') <- load' i m :: Prg (Ref (Expr Float), Maps)
            return (Ex $ Leaf' n', m')


    find  :: Unique -> Node
    find  = fromJust . findNode nodes

    add   :: Unique -> Dynamic -> Map Unique Dynamic -> Map Unique Dynamic
    add   = M.insertWith (P.flip P.const)

    load  :: Unique -> Map Unique Dynamic -> Prg (Dynamic, Map Unique Dynamic)
    load u m = P.flip compileNode m $ find u

    load' :: Typeable n => Unique -> Map Unique Dynamic -> Prg (n, Map Unique Dynamic)
    load' u m = do (d, m') <- load u m
                   case fromDynamic d of
                     Just n  -> return (n, m')
                     Nothing -> error "type error"

        -- (fromJust . fromDynamic) <$> load u m -- I'm a very brave man

-}

--------------------------------------------------------------------------------
-- ...

{-

newRef' :: Typeable a => Expr a -> Prg (Ref (Expr a))
newRef' (Var a) = return (RefComp a :: Ref (Expr a))
newRef' x       = newRef x

fromJust' :: Maybe a -> a
fromJust' (Just a) = a
fromJust' (Nothing) = error "!"

-}

--------------------------------------------------------------------------------
-- * Optimization
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Buffers

-- |
data Buffer exp a = Buffer
  { getBuff :: exp Int -> Program (CMD exp) (exp a)
  , putBuff :: exp a   -> Program (CMD exp) ()
  }

--instance Show (Buffer a) where show _ = "buffer"

{-
newBuff :: exp Int -> exp a -> Program (CMD exp) (Buffer exp a)
newBuff size init = do
  arr <- newArr size init
  ir  <- newRef 0
  let get j = do
        i <- unsafeGetRef ir
        getArr ((i + (size - j - 1)) `mod` size) arr
  let put a = do
        i <- unsafeGetRef ir
        setArr i a arr
        iff (eq i (size-1))
            (setRef ir 0)
            (setRef ir (i + 1))
  return $ Buffer get put
-}

--------------------------------------------------------------------------------
-- *** Finding buffers

{-

delayChains :: (Typeable a)
            => Graph TSignal
            -> Prg ( Map Unique (Buffer a)
                   , Graph TSignal)
delayChains (Graph gnodes groot) = do
    (chains, buffers) <- fmap unzip $ buffer $ chains gnodes
    let nodes = update chains gnodes
        maps  = M.fromList $ zip (map (fst . head) chains) buffers
                  -- buffers are given the same id as the first delay in the chain

    return (maps, Graph nodes groot)

chains :: [Node] -> [Chain]
chains nodes = filter ((>1) . length) $ fmap (flip chain delays) vars
  where
    chain :: Node -> [Node] -> Chain
    chain n@(i, _) xs = n : go i xs
      where
        go _ [] = []
        go u (n@(i, TDelay _ r):ns)
           | u == r    = n : go i xs
           | otherwise = go u ns

    delays = [x | x@(_, TDelay _ _) <- nodes]
    vars   = [x | x <- nodes , not $ isDelay x]
      where
        isDelay (_, TDelay _ _) = True
        isDelay _               = False

buffer :: (Typeable a) => [Chain] -> Prg [(Chain, Buffer a)]
buffer chains = sequence $ fmap (create . update) chains
  where
    update :: (Typeable a) => Chain -> (Chain, [Expr a])
    update ((u, _):xs) =
          -- I assume that a ref for u will exists when we get to the TVBuff node
          -- -u: hack to get unique id, since u is unique and its domain is >=0
        let (chain, delays) = unzip $ snd $ mapAccumL updateNode 1 xs
         in ((-u, TVBuff u) : chain, delays)
      where
        updateNode :: (Typeable a) => Expr Int -> Node -> (Expr Int, (Node, Expr a))
        updateNode n (i, TDelay a _) =
            (n + 1, ((i, TDBuff (-u) n), fromJust $ cast a))

    create :: (Typeable a) => (Chain, [Expr a]) -> Prg (Chain, Buffer a)
    create (ns, as) = do
        -- assumes that each value of as is the same
      buff <- newBuff (Val $ length as) (head as)
      return (ns, buff)

update :: [Chain] -> [Node] -> [Node]
update chains ns =
    let (vars, delays) = fmap (updateDelays ns) $ split chains
     in delays ++ vars -- the order of things doesn't matter here
  where
      -- safe head
    split :: [[Node]] -> ([Node], [Chain])
    split = foldr (\(x,y) -> fmap'' (x:) (y:)) ([],[])
         . map (fmap' head . splitAt 1)

    updateDelays :: [Node] -> [Chain] -> [Node]
    updateDelays = foldr (flip $ foldr replace)

      -- I assume that nodes have a unique id
    replace :: Node -> [Node] -> [Node]
    replace _ [] = []
    replace y (x:xs)
      | fst y == fst x = y : xs
      | otherwise      = x : replace y xs

-}

{-

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

fmap' :: (a -> c) -> (a, b) -> (c, b)
fmap' f p = swap $ fmap f (swap p)
            -- this is one of those arrow operators

fmap'' :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
fmap'' f g = fmap' f . fmap g
            -- this is another one

-}

--------------------------------------------------------------------------------
-- Stuff I'm note sure about yet
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Existensial type

{-

data Ex
  where
    Ex :: Rx a -> Ex

-}

--------------------------------------------------------------------------------
-- Witness

{-

data Wt a
  where
    Wt :: Typeable a => Wt a

ws :: Struct a -> Wt a
ws (Leaf _) = Wt
ws (Pair l r)
   | Wt <- ws l
   , Wt <- ws r
   = Wt

wr :: Rx a -> Wt a
wr (Leaf' _) = Wt
wr (Pair' l r)
   | Wt <- wr l
   , Wt <- wr r
   = Wt

-}

--------------------------------------------------------------------------------
-- Struct with references at the bottom

{-

data Rx a
  where
    Leaf' :: Typeable a => Ref (Expr a) -> Rx (Expr a)
    Pair' :: Rx a -> Rx b -> Rx (a, b)
  deriving Typeable

r2s :: Rx a -> Program (CMD Expr) (Struct a)
r2s (Leaf' r)   = unsafeGetRef r >>= return . Leaf
r2s (Pair' l r) = do
  l' <- r2s l
  r' <- r2s r
  return $ Pair l' r'

s2r :: Struct a -> Program (CMD Expr) (Rx a)
s2r (Leaf e)    = newRef e >>= return . Leaf'
s2r (Pair l r)  = do
  l' <- s2r l
  r' <- s2r r
  return $ Pair' l' r'

-}

--------------------------------------------------------------------------------
