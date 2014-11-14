{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Frontend.SignalComp where

----------------------------------------
import Core
import Expr
import Interpretation
----------------------------------------

import           Frontend.Stream (Stream)
import qualified Frontend.Stream as Str

import           Frontend.Signal (Signal, Sig)
import qualified Frontend.Signal as Sig

import           Frontend.SignalObsv (TSignal(..))
import qualified Frontend.SignalObsv as SigO

import           Data.Map (Map)
import qualified Data.Map as M

import Core                (CMD, Ref, newRef, setRef, getRef, initRef
                               , Arr, newArr, setArr, getArr)
import Expr                (Expr(..))
import Interpretation      (VarPred)
import Frontend.Signal     (Signal)
import Frontend.SignalObsv (TSignal(..))

import qualified Backend.C            -- HACK HACK! Move CompExp instance

import Control.Applicative ((<$>))
import Control.Monad.Operational
import Control.Monad.State
import Data.Dynamic
import Data.Foldable (find)
import Data.Functor.Identity
import Data.List  (mapAccumL)
import Data.Maybe (fromJust)
import Data.Reify
import Data.Proxy
import Data.Typeable

import           Prelude
import qualified Prelude as P

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

compile :: forall a b. (Typeable a, Typeable b)
          =>    (Signal (Expr a)             -> Signal (Expr b))
          -> IO (Program (CMD Expr) (Expr a) -> Program (CMD Expr) (Expr b))
compile f = do
  graph <- reifyGraph f
  return $ \i -> do (buff, graph') <- delayChains graph
                    compileGraph graph' buff i

--------------------------------------------------------------------------------
-- ** Helper types / functions

-- |
type Prg a = Program (CMD Expr) a

-- |
type Node  = (Unique, TSignal Unique)

-- |
type Chain = [Node]

-- |
type Id    = Unique

findNode :: [Node] -> Id -> Maybe Node
findNode nodes i = find ((==) i . P.fst) nodes

--------------------------------------------------------------------------------
-- Nothing to see here citizen, move along.

-- ToDo: Change how references are handeled
-- ToDo: Reader, Writer compiler
-- ToDo: Some other restriction than "Any" on Expr will lead to problems..

compileGraph :: forall a b . (Typeable a, Typeable b)
             => Graph TSignal
             -> Map Unique (Buffer a)
             -> Prg (Expr a)
             -> Prg (Expr b)
compileGraph (Graph nodes root) buffers input = do
    d <- compileNode (fromJust $ findNode nodes root) M.empty

    let x :: Maybe (Ref (Expr b))
        x = fromDynamic d

        y :: Maybe (Struct (Expr b))
        y = fromDynamic d

    case (x, y) of
      (Just r, _) -> getRef r
      (_, Just s) -> case s of (Leaf v) -> return v
      (_, _) -> error $ show $ dynTypeRep d
  where
    compileNode :: Node -> Map Id Dynamic -> Prg Dynamic
    compileNode (i, n) m
      | Just d <- M.lookup i m = return d
      | otherwise              = case n of
          (TVar) -> do
            v <- input
            case v of
              (Var r) -> error "1"
              _       -> do r <- newRef v
                            return $ toDyn r

            -- If I make "Proxy t -> Proxy (Expr t)" then functions
            -- will be limited, more so than for TConst and TLift
          (TLambda (_ :: Proxy t) s u) -> do
            s' <- load s m
            u' <- load u $ add s s' m

            -- the proxy type "t" here represents the desired return value
            -- of the function. Which will either be a reference to such a
            -- value, or a struct of the value.
            let x :: Maybe (Ref t)
                x = fromDynamic u'

                y :: Maybe (Struct t)
                y = fromDynamic u'

            case (x, y) of
              (Just r, _) -> return $ toDyn r
              (_, Just r) -> return $ toDyn r

          (TConst s) -> do
            s' <- Str.runStream s
            r  <- newRef s'
            return $ toDyn r

          (TLift (f :: Stream (Expr t0) -> Stream (Expr t1)) s) -> do
            r  <- initRef                     :: Prg (Ref (Expr t1))
            s' <- load' s (add i (toDyn r) m) :: Prg (Ref (Expr t0))
            v  <- Str.runStream $ f $ Str.Stream $ return $ getRef s'
            setRef r v
            return $ toDyn r

          (TMap (f :: Struct t0 -> Struct t1) s) -> do
            Ex r <- compileStruct (find s) m
            s'   <- r2s r

            -- hm...
            let y = case ws s' of
                      Wt -> case cast s' of
                              Just x  -> f x
                              Nothing -> error "!"

            return $ toDyn y

          (TVBuff s) -> do
            let buff = case M.lookup s buffers of
                         Just b  -> b
                         Nothing -> error $ "couldn't find buffer: " ++ show s
                                         ++ "in " ++ show buffers
            v <- input :: Program (CMD Expr) (Expr a)
            putBuff buff v
            case v of
              (Var r) -> return $ toDyn $ (Ref r :: Ref (Expr a))
              _       -> do r <- newRef v
                            return $ toDyn r
          (TDBuff i s) -> do
            let buff = case M.lookup s buffers of
                         Just b  -> b
                         Nothing -> error $ "couldn't find buffer: " ++ show s
                                         ++ " in " ++ show buffers
            (Var v) <- getBuff buff i :: Program (CMD Expr) (Expr a)
            return $ toDyn $ (Ref v   :: Ref (Expr a))

          (TDelay v s) -> do
             -- Make global
            r <- newRef $ litExp True
            r <- newRef $ v

            error "why are you here?.."

    compileStruct :: Node -> Map Id Dynamic -> Prg Ex
    compileStruct (i, n) m = case n of
          (TZip l r) -> do
            Ex l' <- compileStruct (find l) m
            Ex r' <- compileStruct (find r) m
            return $ Ex $ Pair' l' r'
          (TFst l) -> do
            Ex l' <- compileStruct (find l) m
            case l' of
              Pair' x _ -> return $ Ex x
          (TSnd r) -> do
            Ex r' <- compileStruct (find r) m
            case r' of
              Pair' _ x -> return $ Ex x
          (TMap (f :: Struct t0 -> Struct t1) s) -> do
            n' <- load' i m :: Prg (Struct t1)
            r' <- s2r n'
            return $ Ex r'
          _  -> do
            n' <- load' i m :: Prg (Ref (Expr Float))
            return $ Ex $ Leaf' n'

    find  :: Unique -> Node
    find  = fromJust . findNode nodes

    add   :: Unique -> Dynamic -> Map Unique Dynamic -> Map Unique Dynamic
    add   = M.insertWith (P.flip P.const)

    load  :: Unique -> Map Unique Dynamic -> Program (CMD Expr) Dynamic
    load u m = P.flip compileNode m $ find u

    load' :: Typeable n => Unique -> Map Unique Dynamic -> Program (CMD Expr) n
    load' u m = (fromJust . fromDynamic) <$> load u m -- I'm a very brave man


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

newBuff :: Expr Int -> Expr a -> Prg (Buffer a)
newBuff size init = do
  arr <- newArr size init
  ir  <- newRef 0
  let get j = do
        i <- unsafeGetRef ir
        getArr ((size + i + j - 1) `mod` size) arr
  let put a = do
        i <- unsafeGetRef ir
        setRef ir ((i + 1) `mod` size)
        setArr i a arr
  return $ Buffer get put

getBuffer :: Expr Int -> Buffer a -> Prg (Expr a)
getBuffer = flip getBuff

putBuffer :: Expr a -> Buffer a -> Prg ()
putBuffer = flip putBuff

--------------------------------------------------------------------------------
-- *** Finding buffers

delayChains :: (Typeable a)
            => Graph TSignal
            -> Prg ( Map Unique (Buffer a)
                   , Graph TSignal)
delayChains (Graph gnodes groot) = do
    (chains, buffers) <- fmap unzip $ buffer $ chains gnodes
    let nodes = update chains gnodes
        maps  = M.fromList $ zip (map (fst . head) chains) buffers
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

    vars   = [x | x@(_, TVar)       <- nodes]
    delays = [x | x@(_, TDelay _ _) <- nodes]

buffer :: (Typeable a) => [Chain] -> Prg [(Chain, Buffer a)]
buffer chains = sequence $ fmap (create . update) chains
  where
    update :: (Typeable a) => Chain -> (Chain, [Expr a])
    update ((u, TVar):xs) =
        let (chain, delays) = unzip $ snd $ mapAccumL f 1 xs
         in ((u, TVBuff u) : chain, delays)
      where f n (i, TDelay a _) = (n + 1, ( (i, TDBuff n u)
                                          , case cast a of
                                              Just x  -> x
                                              Nothing -> error "typ buff err."))

    create :: (Typeable a) => (Chain, [Expr a]) -> Prg (Chain, Buffer a)
    create (ns, as) = do
      buff <- newBuff (Val $ length as) (head as)
      return (ns, buff)

update :: [Chain] -> [Node] -> [Node]
update = flip $ foldr (flip $ foldr replace)
  where
    replace :: Node -> [Node] -> [Node]
    replace _ [] = []
    replace y (x:xs)
      | fst y == fst x = y : xs
      | otherwise      = x : replace y xs

--------------------------------------------------------------------------------
-- Stuff I'm note sure about yet
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Existensial type

data Ex
  where
    Ex :: Rx a -> Ex

--------------------------------------------------------------------------------
-- Witness

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

--------------------------------------------------------------------------------
-- Struct with references at the bottom

data Rx a
  where
    Leaf' :: Typeable a => Ref (Expr a) -> Rx (Expr a)
    Pair' :: Rx a -> Rx b -> Rx (a, b)
  deriving Typeable

r2s :: Rx a -> Program (CMD Expr) (Struct a)
--r2s (Leaf' r) = return . Leaf . Var $ C.unRef r
r2s (Leaf' r)   = unsafeGetRef r >>= return . Leaf
r2s (Pair' l r) = do
  l' <- r2s l
  r' <- r2s r
  return $ Pair l' r'

s2r :: Struct a -> Program (CMD Expr) (Rx a)
s2r (Leaf e)       = newRef e >>= return . Leaf'
s2r (Pair l r)     = do
  l' <- s2r l
  r' <- s2r r
  return $ Pair' l' r'

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

fir :: [Expr Float] -> Sig Float -> Sig Float
fir as = sums . muls as . delays ds
  where
    ds = replicate (length as) 0

    sums :: [Sig Float] -> Sig Float
    sums = foldr1 (+)

    muls :: [Expr Float] -> [Sig Float] -> [Sig Float]
    muls as = zipWith (*) (map Sig.repeat as)

    delays :: [Expr Float] -> Sig Float -> [Sig Float]
    delays as s = tail $ scanl (flip Sig.delay) s as

tGraph :: IO (Graph TSignal)
tGraph = reifyGraph $ fir [1.1, 1.2, 1.3]

-- chains : Ok

tBuffer = do
  (Graph gnodes groot) <- tGraph
  return $ do
    let c   = chains gnodes
        cb  = buffer c
        cb' = unzip <$> cb
    (cs, b) <- cb' :: Program (CMD Expr) ([Chain], [Buffer Float])
    return "!"
