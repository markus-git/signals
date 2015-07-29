{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Compiler.Linker {-(
    Resolution(..)
  , TEx
  , linker
  )-}
where

import Core

import Frontend.Stream 
import Frontend.Signal hiding (pack, unpack, lift)
import Frontend.Signal.Observ

import Backend.Knot

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Data.Ref
import Data.Ref.Map (Map, Name)

import qualified Data.Ref.Map as M

import Prelude hiding (Left, Right)

-- Temp !
import System.Mem.StableName (eqStableName)
import Unsafe.Coerce
-- !

--------------------------------------------------------------------------------
-- * Linking
--------------------------------------------------------------------------------

type family Refer (i :: (* -> *) -> * -> *) (a :: *) :: *
type instance Refer i (S Symbol i (Identity a)) = Name (S Symbol i (Identity a))
type instance Refer i (S Symbol i (a, b))       = (Refer i a, Refer i b)

data Tuple i a where
  Leaf :: Name    (S Symbol i (Identity a))
       -> Tuple i (S Symbol i (Identity a))
          
  Pair :: Tuple i a
       -> Tuple i b
       -> Tuple i (a, b)

--------------------------------------------------------------------------------

pack :: forall i a. Witness a => Refer i (S Symbol i a) -> Tuple i (S Symbol i a)
pack = undefined
  where
    go :: Wit a -> Refer i (S Symbol i a) -> Tuple i (S Symbol i a)
    go (WE)     s      = Leaf s
    go (WP _ _) (l, r) = Pair (pack l) (pack r)

--------------------------------------------------------------------------------
{-
-- | ... I assume each index yeilds a tree with the expected type
data Resolution i = Resolution {
    _input  :: ()
  , _output :: Map (Entry i)
  }

-- | Constraints over symbol to input/output tree
data Constraint i
  = In  ()
  | Out (forall a. (Name a, Entry i a))

-- | ...
type M (i :: (* -> *) -> * -> *) = Knot (Resolution i) (Constraint i) (State (Map (Node i)))
-}
--------------------------------------------------------------------------------
{-
-- | Attempts to fetch resolved value of index
resolve :: Name a -> M i (Entry i a)
resolve name = asks $ (M.! name) . _output

-- | Adds an output constraint
constrain :: Name a -> Entry i a -> M i ()
constrain name entry = tell [Out $ unsafeCoerce (name, entry)]

-- | Adds an input constraint
introduce :: Name a -> Entry i a -> M i ()
introduce name entry = tell []
-}
--------------------------------------------------------------------------------
{-
link' :: Witness a => Name (S Symbol i a) -> M i ()
link' name =
  do (Node n) <- gets $ (M.! name)
     () <- case n of
       (Repeat c) ->
         do constrain name (pack name)
       (Map f (Key s)) ->
         do t <- resolve s
            constrain name (pack name)
       (Join (Key l) (Key r)) ->
         do t_l <- resolve l
            t_r <- resolve r
            constrain name (Entry (t_l, t_r))
            undefined
     undefined
  where
    pack :: forall i a. Witness a => Name (S Symbol i a) -> Entry i (S Symbol i a)
    pack n = Entry $ go (wit :: Wit a) n
      where
        go :: Wit a -> Name (S Symbol i a) -> Refer i (S Symbol i a)
        go (WE)     s = s
        go (WP _ _) s = error "link: I have no idea what to put here"

-}{-
-- | Given a signal node, link creates constraints modeling its relation to others
link :: forall m i instr. (Monad m, Ord i, Show i, Typeable instr)
     => (i, TSignal instr i)
     -> Knot (Resolution i instr)
             (Constraint i instr) m
             ()

link (i, TLambda l r) =
  do return ()

link (i, TVar t) =
  do constrain i $ mark (show i) t

link (i, TConst (c :: Stream instr (IExp instr a))) =
  do constrain i (Seaf (show i) :: Suple instr (Empty instr a)) 

link (i, TLift (f :: Stream instr (IExp instr a) -> Stream instr (IExp instr b)) s) =
  do let t = undefined :: Suple instr (Empty instr a)
     t' <- resolve s t
     introduce i t'
     constrain i (Seaf (show i) :: Suple instr (Empty instr a))

link (i, TDelay (e :: IExp instr a) s) =
  do let t = undefined :: Suple instr (Empty instr a)
     t' <- resolve s t
     introduce i t'
     constrain i (Seaf (show i) :: Suple instr (Empty instr a))

link (i, TMap ti to f s) =
  do t' <- resolve s ti
     introduce i t'
     constrain i $ mark (show i) to

link (i, TJoin tl tr l r) =
  do tl' <- resolve l tl
     tr' <- resolve r tr
     constrain i $ Sranch tl' tr'

link (i, TLeft t l) =
  do t' <- resolve l t
     constrain i $ lefty t'
  where
    lefty :: Suple instr (a, b) -> Suple instr a
    lefty t = case t of (Sranch l r) -> l

link (i, TRight t r) =
  do t' <- resolve r t
     constrain i $ righty t
  where
    righty :: Suple instr (a, b) -> Suple instr b
    righty t = case t of (Sranch l r) -> r
-}

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
{-
-- | ...
linker :: Typeable instr => [(Unique, Node instr)] -> Resolution Unique instr
linker = snd . runIdentity . tie solve . sequence . fmap link

-- | ...
solve :: Solver (Resolution Unique instr) (Constraint Unique instr)
solve constraints =
  let inputs  = [ i | In  i <- constraints]
      outputs = [ o | Out o <- constraints]
  in  Resolution
       { _output = M.fromList outputs
       , _input  = M.fromList inputs
       }
-}







--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


{-
resolve :: (MonadReader (Resolution i instr) m, Ord i, Typeable a) => i -> Suple instr a -> m (Suple instr a)
resolve i _ =
  do ex <- asks ((! i) . _output)
     return $ case ex of
       Ex t -> case gcast t of
         Nothing -> error "resolve: type error"
         Just o  -> o

-- | Mark tree with index
mark :: String -> Suple instr a -> Suple instr a
mark s (Seaf   _)   = Seaf s
mark s (Sranch l r) = Sranch (mark (s ++ "_l") l) (mark (s ++ "_r") r)

constrain :: (MonadWriter [Constraint i instr] m, Typeable a) => i -> Suple instr a -> m ()
constrain i t = tell [Out (i, Ex t)]

introduce :: (MonadWriter [Constraint i instr] m, Typeable a) => i -> Suple instr a -> m ()
introduce i t = tell [In  (i, Ex t)]
-}
