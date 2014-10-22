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
import qualified Unsafe.Coerce as BAD -- HACK HACK! Needs ghc >7.8 for Typeable Prog.

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
compile f = do
  g <- reifyGraph f
  return $ \input -> compileG g input

--------------------------------------------------------------------------------
-- **

-- |
type Node = (Unique, TSignal Unique)

-- |
type Root = Unique

-- |
type Id   = Unique

findNode :: [Node] -> Id -> Maybe Node
findNode nodes i = find ((==) i . P.fst) nodes

--------------------------------------------------------------------------------
-- Nothing to see here citizen, move along.


-- ToDo: Change how references are handeled
-- ToDo: Reader, Writer compiler
-- ToDo: Some other restriction than "Any" on Expr will lead to problems..

compileG :: forall a b. (Typeable a, Typeable b)
           => Graph TSignal
           -> Program (CMD Expr) a
           -> Program (CMD Expr) b
compileG (Graph nodes root) inp =
  do x <- castN root M.empty
     v <- getRef x            :: Program (CMD Expr) (Expr b)
     return (fromJust (cast v :: Maybe b))
  where
    compileN :: Node -> Map Id Dynamic -> Program (CMD Expr) Dynamic
    compileN (i, n) m
      | Just d <- M.lookup i m = return d
      | otherwise              = case n of
          (TVar) -> do
            v <- inp                         :: Program (CMD Expr) a
            r <- newRef $ fromJust $ (cast v :: Maybe (Expr a))
            return $ toDyn r

          (TLambda (_ :: Proxy t) x y) -> do
            r  <- initRef :: Program (CMD Expr) (Ref (Expr t))
            x' <- loadN x (addN i (toDyn r) m)
            y' <- castN y (addN x x'        m)

            v  <- getRef y'
            setRef r v
            return $ toDyn r

          (TConst (x :: Stream t)) -> do
            x' <- Str.runStream x             :: Program (CMD Expr) t
            r  <- newRef $ fromJust $ cast x' :: Program (CMD Expr) (Ref (Expr t))
            return $ toDyn r

          (TLift (f :: Stream t1 -> Stream t2) x) -> do
            r  <- initRef --                   :: Program (CMD Expr) (Ref t2)
            x' <- castN x (addN i undefined m) :: Program (CMD Expr) (Ref t1)

            -- I really need to change to type of getRef...
            let apa :: Program (CMD Expr) t1
                apa = BAD.unsafeCoerce
                         ( getRef
                         $ fromJust
                         $ cast x' :: Program (CMD Expr) (Expr t1))

                bepa :: Program (CMD Expr) t2
                bepa = Str.runStream $ f $ Str.Stream $ return apa

            v <- bepa
            setRef r (fromJust $ cast v :: Expr t2)
            return $ toDyn r

          (TMap (f :: Struct t1 -> Struct t2) r) -> do
            let u = "a"             :: VarId
            let o = f (varStruct u) :: Struct t2

            s <- compS o
            r <- initRef :: Program (CMD Expr) (Ref (Expr t2))
            return $ toDyn r
            where
              compS :: Struct t2 -> Program (CMD Expr) (Struct (Ref (Expr t2)))
              compS = undefined

    find :: Unique -> Node
    find = fromJust . findNode nodes

    addN :: Unique -> Dynamic -> Map Unique Dynamic -> Map Unique Dynamic
    addN = M.insertWith (P.flip P.const)

    loadN :: Unique -> Map Unique Dynamic -> Program (CMD Expr) Dynamic
    loadN u m = P.flip compileN m $ find u

    castN :: Typeable n => Unique -> Map Unique Dynamic -> Program (CMD Expr) n
    castN u m = (fromJust . fromDynamic) <$> loadN u m
