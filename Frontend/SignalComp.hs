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

import           Frontend.Stream (Stream(..), Str)
import qualified Frontend.Stream as Str

import           Frontend.Signal (Signal, Sig, Empty(..), Struct(..), TStruct(..), rep)
import qualified Frontend.Signal as Sig

import           Frontend.SignalObsv (TSignal(..))
import qualified Frontend.SignalObsv as SigO

import           Data.Map (Map)
import qualified Data.Map as M

import Core (CMD)

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Operational

import           Control.Monad.Writer (WriterT, MonadWriter)
import qualified Control.Monad.Writer as CMW

import           Control.Monad.Reader (ReaderT, MonadReader, MonadTrans, lift)
import qualified Control.Monad.Reader as CMR

import           Control.Monad.Identity (Identity)
import qualified Control.Monad.Identity as CMI

import Data.Dynamic
import Data.List (find)
import Data.Reify
import Data.Proxy
import Data.Typeable

import           Prelude
import qualified Prelude as P

---------------------------------------- : Testing
import qualified Expr as E
---------------------------------------- : End

--------------------------------------------------------------------------------
-- * Knot Monad
--------------------------------------------------------------------------------

newtype KnotT i x m a =
    KnotT { unKnotT :: ReaderT (Map i x) (WriterT [(i, x)] m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Map i x)
    , MonadWriter [(i, x)]
    )

type Knot i x = KnotT i x Identity

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

solve :: Ord i => [(i, x)] -> Map i x
solve = M.fromList

tie :: (Ord i, MonadFix m) => KnotT i x m a -> m (a, Map i x)
tie (KnotT knot) = mfix $ \(a, solution) ->
  do (a, constraints) <- CMW.runWriterT $ CMR.runReaderT knot solution
     let solution = solve constraints
     return (a, solution)

--------------------------------------------------------------------------------
-- * Compiler
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** Linker

type Id  = String

type Ref = String

linker :: Graph (TSignal exp) -> Map Id Ref
linker (Graph ns _) = snd
                    $ CMI.runIdentity
                    $ tie
                    $ sequence
                    $ fmap (link . showP) ns
  where
    showP (x, y) = (show x, showTS y)

-- | ...
link :: (Id, TSignal exp Ref) -> Knot Id Ref ()

link (i, TLambda l r) =
  do knot l
     knot r
     i =: ('?' : i)

link (i, TVar) =
  do i =: "input"

link (i, TConst _) =
  do i =: ('v' : i)

link (i, TLift _ s) =
  do knot s
     i =: ('v' : i)

link (i, TMap t _ s) =
  do asks t s
     i =: ('?' : i)

link (i, TZip t t' l r) =
  do sl <- asks t  l
     sr <- asks t' r
     tells (TPair sl sr) i

link (i, TFst t l) =
  do (TPair sl _) <- asks t l
     tells sl i

link (i, TSnd t r) =
  do (TPair _ sr) <- asks t r
     tells sr i

-- | ...
showTS :: Show r => TSignal exp r -> TSignal exp String
showTS node =
  case node of
    (TLambda x y)   -> TLambda   (show x) (show y)
    (TVar)          -> TVar
    (TConst e)      -> TConst e
    (TLift f x)     -> TLift f   (show x)
    (TMap t f x)    -> TMap t f  (show x)
    (TZip t t' x y) -> TZip t t' (show x) (show y)
    (TFst t x)      -> TFst t    (show x)
    (TSnd t x)      -> TSnd t    (show x)
    (TDelay e x)    -> TDelay e  (show x)

----------------------------------------

asks :: TStruct exp a -> String -> Knot Id Ref (TStruct exp a)
asks (TLeaf i)   s = knot s >>= return . TLeaf
asks (TPair l r) s =
  do l' <- asks l $ s ++ "_1"
     r' <- asks r $ s ++ "_2"
     return $ TPair l' r'

tells :: TStruct exp a -> String -> Knot Id Ref ()
tells (TLeaf i)   s = i =: s
tells (TPair l r) s =
  do tells l $ s ++ "_1"
     tells r $ s ++ "_2"

{-

-- | does some unwrapping/wrapping of stream functions
streamify   :: (Str exp a -> Str exp b) -> exp a -> SProg exp b
streamify f = Str.run . f . Stream . return . return

-}

--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

signal :: Sig E.Expr Float -> Sig E.Expr Float
signal s = s + (Sig.repeat 1)
