{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.SignalComp where

import Expr

import           Frontend.Stream (Stream)
import qualified Frontend.Stream as Str

import           Frontend.Signal (Signal)
import qualified Frontend.Signal as Sig

import           Frontend.SignalObsv (TSignal(..))
import qualified Frontend.SignalObsv as SigO

import           Data.Map (Map)
import qualified Data.Map as M

import Core (CMD, Ref, newRef, setRef, getRef, initRef)
import Expr (Expr(..))
import Interpretation (VarPred)
import Frontend.Signal     (Signal)
import Frontend.SignalObsv (TSignal(..))

import qualified Backend.C            -- HACK HACK! Move CompExp instance

import Control.Applicative ((<$>))
import Control.Monad.Operational
import Control.Monad.State
import Data.Dynamic
import Data.Foldable (find)
import Data.Functor.Identity
import Data.Maybe (fromJust)
import Data.Reify
import Data.Proxy
import Data.Typeable

import           Prelude hiding (const, fst, snd, map, repeat, zipWith)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

compile :: forall a b. (Typeable a, Typeable b)
          =>    (Signal a             -> Signal b)
          -> IO (Program (CMD Expr) a -> Program (CMD Expr) b)
compile f = undefined
            {-
  g <- reifyGraph f
  return $ \input -> compileGraph g input
-}

--------------------------------------------------------------------------------
-- ** Helper types / functions

-- |
type Node = (Unique, TSignal Unique)

-- |
type Id   = Unique

findNode :: [Node] -> Id -> Maybe Node
findNode nodes i = find ((==) i . P.fst) nodes

--------------------------------------------------------------------------------
-- Nothing to see here citizen, move along.

-- ToDo: Change how references are handeled
-- ToDo: Reader, Writer compiler
-- ToDo: Some other restriction than "Any" on Expr will lead to problems..

compileGraph :: forall a b . (Typeable a, Typeable b)
               => Graph TSignal
               -> Program (CMD Expr) (Expr a)
               -> Program (CMD Expr) (Expr b)
compileGraph (Graph nodes root) input = do
    d <- compileNode (fromJust $ findNode nodes root) M.empty
    let v = case fromDynamic d of
              Just x  -> x
              Nothing -> error $ "..."
    getRef v
  where
    compileNode :: Node -> Map Id Dynamic -> Program (CMD Expr) Dynamic
    compileNode (i, n) m
      | Just d <- M.lookup i m = return d
      | otherwise              = case n of
          (TVar) -> do
            v <- input
            r <- newRef v
            return $ toDyn r

            -- If I make "Proxy t -> Proxy (Expr t)" then functions
            -- will be limited, more so than for TConst and TLift
          (TLambda (_ :: Proxy t) s u) -> do
            s' <- load  s m
            u' <- load' u $ add s s' m :: Program (CMD Expr) (Ref (Expr Float))
            return $ toDyn u'

          (TConst s) -> do
            s' <- Str.runStream s
            r  <- newRef s'
            return $ toDyn r

          (TLift (f :: Stream (Expr t0) -> Stream (Expr t1)) s) -> do
            r  <- initRef                     :: Program (CMD Expr) (Ref (Expr t1))
            s' <- load' s (add i (toDyn r) m) :: Program (CMD Expr) (Ref (Expr t0))
            v  <- Str.runStream $ f $ Str.Stream $ return $ getRef s'
            setRef r v
            return $ toDyn r

          (TMap (f :: Struct t0 -> Struct t1) s) -> do
            Ex s' <- compileStruct (find s) m

            let x = case wt s' of
                      Wt -> case cast s' of
                              Just y  -> f y
                              Nothing -> error "..."

            flattenStruct x
            return $ toDyn x

    compileStruct :: Node -> Map Id Dynamic -> Program (CMD Expr) Ex
    compileStruct (i, n) m = case n of
          (TZip l r) -> do
            Ex l' <- compileStruct (find l) m
            Ex r' <- compileStruct (find r) m
            return $ Ex $ Pair l' r'
          (TFst l)   -> do
            Ex l' <- compileStruct (find l) m
            case l' of
              Pair x _ -> return $ Ex x
          (TSnd r)   -> do
            Ex r' <- compileStruct (find r) m
            case r' of
              Pair _ x -> return $ Ex x
          (TMap (f :: Struct t0 -> Struct t1) s) -> do
            n' <- load' i m :: Program (CMD Expr) (Struct t1)
            return $ Ex n'
          _          -> do
            n' <- load' i m :: Program (CMD Expr) (Ref (Expr Float))
            r  <- getRef n' -- This is probably wrong
            return $ Ex $ Leaf r

    flattenStruct :: Struct t -> Program (CMD Expr) ()
    flattenStruct (Leaf e)   = newRef e >> return ()
    flattenStruct (Pair l r) = do
      flattenStruct l
      flattenStruct r
      return ()

    find  :: Unique -> Node
    find  = fromJust . findNode nodes

    add   :: Unique -> Dynamic -> Map Unique Dynamic -> Map Unique Dynamic
    add   = M.insertWith (P.flip P.const)

    load  :: Unique -> Map Unique Dynamic -> Program (CMD Expr) Dynamic
    load u m = P.flip compileNode m $ find u

    load' :: Typeable n => Unique -> Map Unique Dynamic -> Program (CMD Expr) n
    load' u m = do dyn <- load u m
                   case fromDynamic dyn of
                     Just n  -> return n
                     Nothing -> error $ "load failed on: " ++ show u

                --(fromJust . fromDynamic) <$> load u m


--------------------------------------------------------------------------------
--

data Ex
  where
    Ex :: Struct a -> Ex

data Wt a
  where
    Wt :: Typeable a => Wt a

wt :: Struct a -> Wt a
wt (Leaf _) = Wt
wt (Pair l r)
   | Wt <- wt l
   , Wt <- wt r
   = Wt
