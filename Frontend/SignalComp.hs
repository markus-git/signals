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
    let v = fromJust (fromDynamic d)
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
            -- will be limited, more than for TConst and TLift
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

            undefined

      where
       find  :: Unique -> Node
       find  = fromJust . findNode nodes

       add   :: Unique -> Dynamic -> Map Unique Dynamic -> Map Unique Dynamic
       add   = M.insertWith (P.flip P.const)

       load  :: Unique -> Map Unique Dynamic -> Program (CMD Expr) Dynamic
       load u m = P.flip compileNode m $ find u

       load' :: Typeable n => Unique -> Map Unique Dynamic -> Program (CMD Expr) n
       load' u m = (fromJust . fromDynamic) <$> load u m

--------------------------------------------------------------------------------
-- ***
