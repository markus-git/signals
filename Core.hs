{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Core where

import Data.Constraint
import Control.Monad.Operational

import Interpretation

--------------------------------------------------------------------------------
-- * Commands
--------------------------------------------------------------------------------

-- | Imperative commands
data CMD exp a
  where
    -- ^ File management
    Open  :: FilePath         -> CMD exp Ptr
    Close :: Ptr              -> CMD exp ()
    Put   :: Ptr -> exp Float -> CMD exp ()
    Get   :: Ptr              -> CMD exp (exp Float)

-- |
newtype Ptr = Ptr {unPtr :: String}

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
