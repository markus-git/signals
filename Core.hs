{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DeriveDataTypeable     #-}

module Core where

import Interpretation

import Control.Monad.Operational
import Data.Constraint
import Data.Dynamic

--------------------------------------------------------------------------------
-- * Commands
--------------------------------------------------------------------------------

-- | Imperative commands
data CMD exp a
  where
    -- ^ File management (IOHandler in Haskell)
    Open  :: FilePath         -> CMD exp Ptr
    Close :: Ptr              -> CMD exp ()
    Put   :: Ptr -> exp Float -> CMD exp ()
    Get   :: Ptr              -> CMD exp (exp Float)

    -- ^ Mutable references (IORef in Haskell)
    InitRef :: VarPred exp a =>                         CMD exp (Ref (exp a))
    NewRef  :: VarPred exp a => exp a                -> CMD exp (Ref (exp a))
    GetRef  :: VarPred exp a => Ref (exp a)          -> CMD exp (exp a)
    SetRef  :: VarPred exp a => Ref (exp a) -> exp a -> CMD exp ()

    -- ^ Mutable arrays (IOArray in Haskell)
    NewArr
      :: (Num (exp n), VarPred exp a)
      => exp n -> exp a -> CMD exp (Arr (exp a))
    GetArr
      :: (Num (exp n), VarPred exp a)
      => exp n -> Arr (exp a) -> CMD exp (exp a)
    SetArr
      :: (Num (exp n), VarPred exp a)
      => exp n -> exp a -> Arr (exp a) -> CMD exp ()

-- |
newtype Ptr   = Ptr {unPtr :: String} deriving Typeable

-- |
newtype Ref a = Ref {unRef :: String} deriving Typeable

-- |
newtype Arr a = Arr {unArr :: String} deriving Typeable

--------------------------------------------------------------------------------
-- **

open :: FilePath -> Program (CMD exp) Ptr
open = singleton . Open

close :: Ptr -> Program (CMD exp) ()
close = singleton . Close

fput :: Ptr -> exp Float -> Program (CMD exp) ()
fput p = singleton . Put p

fget :: Ptr -> Program (CMD exp) (exp Float)
fget = singleton . Get

----------------------------------------

initRef :: VarPred exp a => Program (CMD exp) (Ref (exp a))
initRef = singleton InitRef

newRef :: VarPred exp a => exp a -> Program (CMD exp) (Ref (exp a))
newRef = singleton . NewRef

getRef :: VarPred exp a => Ref (exp a) -> Program (CMD exp) (exp a)
getRef = singleton . GetRef

setRef :: VarPred exp a => Ref (exp a) -> exp a -> Program (CMD exp) ()
setRef r = singleton . SetRef r

modifyRef :: VarPred exp a => Ref (exp a) -> (exp a -> exp a) -> Program (CMD exp) ()
modifyRef r f = getRef r >>= setRef r . f

----------------------------------------

newArr
  :: (Num (exp n), VarPred exp a)
  => exp n -> exp a -> Program (CMD exp) (Arr (exp a))
newArr n = singleton . NewArr n

getArr
  :: (Num (exp n), VarPred exp a)
  => exp n -> Arr (exp a) -> Program (CMD exp) (exp a)
getArr n = singleton . GetArr n

setArr
  :: (Num (exp n), VarPred exp a)
  => exp n -> exp a -> Arr (exp a) -> Program (CMD exp) ()
setArr n a = singleton . SetArr n a

--------------------------------------------------------------------------------
-- * Constructs
--------------------------------------------------------------------------------

-- |
data Construct cmd a
  where
    Function :: String -> Program cmd () -> Construct cmd ()

--------------------------------------------------------------------------------
-- **

mkFunction :: String -> Program cmd () -> Program (Construct cmd) ()
mkFunction fun body = singleton $ Function fun body

--------------------------------------------------------------------------------
-- * Run Functions
--------------------------------------------------------------------------------

runCMD :: (EvalExp exp, LitPred exp Float) => CMD exp a -> IO a
runCMD = undefined
