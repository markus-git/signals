{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Compiler.Linker (
    Resolution(..)
  , EStruct(..)
  , linker
  )
where

import Frontend.Stream     (Stream)
import Frontend.Signal     (TStruct(..), Struct, Empty, tpair, tleft, tright, tleaf)
import Frontend.SignalObsv (TSignal(..), Node)

import Backend.Knot

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity

import           Data.Map (Map, (!))
import qualified Data.Map as M

import Data.Reify    (Unique, Graph(..), reifyGraph)
import Data.Typeable

--------------------------------------------------------------------------------
-- * Linking
--------------------------------------------------------------------------------

-- | Untyped binary tree
data EStruct exp
  where
    Ex :: Typeable a => TStruct exp a -> EStruct exp

instance Show (EStruct e) where show _ = "Ex"

-- | ... I assume each index yeilds a tree with the expected type
data Resolution symbol exp = Resolution
  { _output :: Map symbol (EStruct exp)
  , _input  :: Map symbol (EStruct exp)
  }

-- | Constraints over symbol to input/output tree
data Constraint symbol exp
  = In  (symbol, EStruct exp)
  | Out (symbol, EStruct exp)

--------------------------------------------------------------------------------

-- | Attempts to fetch resolved value of index
resolve :: (MonadReader (Resolution i exp) m, Ord i, Typeable a) => i -> m (TStruct exp a)
resolve i =
  do ex <- asks ((! i) . _output)
     return $ case ex of
       Ex t -> case gcast t of
         Nothing -> error "resolve: type error"
         Just o  -> o

-- | Mark tree with index
mark :: String -> TStruct exp a -> TStruct exp a
mark s (TLeaf _)   = TLeaf (s)
mark s (TPair l r) = TPair (mark (s ++ "_l") l) (mark (s ++ "_r") r)

-- | Adds an output constraint
constrain :: (MonadWriter [Constraint i exp] m, Typeable a) => i -> TStruct exp a -> m ()
constrain i t = tell [Out (i, Ex t)]

-- | Adds an input constraint
introduce :: (MonadWriter [Constraint i exp] m, Typeable a) => i -> TStruct exp a -> m ()
introduce i t = tell [In  (i, Ex t)]

--------------------------------------------------------------------------------

-- | Given a signal node, link creates constraints modeling its relation to others
link :: forall m i exp. (Monad m, Ord i, Show i)
     => (i, TSignal exp i)
     -> Knot (Resolution i exp)
             (Constraint i exp) m
             ()

link (i, TLambda l r) =
  do return ()

link (i, TVar t) =
  do constrain i $ mark (show i) t

link (i, TConst (c :: Stream exp (exp a))) =
  do constrain i (tleaf (show i) :: TStruct exp (Empty (exp a))) 

link (i, TLift (f :: Stream exp (exp a) -> Stream exp (exp b)) s) =
  do let t = undefined :: TStruct exp (Empty (exp a))
     t' <- liftM (`asTypeOf` t) $ resolve s
     introduce i t'
     constrain i (tleaf (show i) :: TStruct exp (Empty (exp b)))

link (i, TDelay (e :: exp a) s) =
  do let t = undefined :: TStruct exp (Empty (exp a))
     t' <- liftM (`asTypeOf` t) $ resolve s
     introduce i t'
     constrain i (tleaf (show i) :: TStruct exp (Empty (exp a)))

link (i, TMap ti to f s) =
  do t' <- liftM (`asTypeOf` ti) $ resolve s
     introduce i t'
     constrain i $ mark (show i) to

link (i, TZip tl tr l r) =
  do tl' <- liftM (`asTypeOf` tl) $ resolve l
     tr' <- liftM (`asTypeOf` tr) $ resolve r
     constrain i $ tpair tl' tr'

link (i, TFst t l) =
  do t' <- liftM (`asTypeOf` t) $ resolve l
     constrain i $ tleft t'

link (i, TSnd t r) =
  do t' <- liftM (`asTypeOf` t) $ resolve r
     constrain i $ tright t'

--------------------------------------------------------------------------------

-- | ...
linker :: [(Unique, Node exp)] -> Resolution Unique exp
linker = snd . runIdentity . tie solve . sequence . fmap link

-- | ...
solve :: Solver (Resolution Unique exp) (Constraint Unique exp)
solve constraints =
  let inputs  = [ i | In  i <- constraints]
      outputs = [ o | Out o <- constraints]
  in  Resolution
       { _output = M.fromList outputs
       , _input  = M.fromList inputs
       }
