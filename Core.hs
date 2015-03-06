{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Core where

import Interpretation

import Control.Monad.Operational
import Data.Constraint
import Data.Dynamic
import Data.Typeable
import Data.IORef
import Data.Array.IO.Safe
import qualified System.IO as IO

--------------------------------------------------------------------------------
-- * Commands
--------------------------------------------------------------------------------

-- | Imperative commands
data CMD exp a
  where
    -- ^ File management    (IOHandler in Haskell)
    Open  :: FilePath            -> CMD exp Handle
    Close :: Handle              -> CMD exp ()
    Put   :: Handle -> exp Float -> CMD exp ()
    Get   :: Handle              -> CMD exp (exp Float)
    Eof   :: Handle              -> CMD exp (exp Bool)

    -- ^ Mutable references (IORef in Haskell)
    NewRef  :: exp a                -> CMD exp (Ref (exp a))
    GetRef  :: Ref (exp a)          -> CMD exp (exp a)
    SetRef  :: Ref (exp a) -> exp a -> CMD exp ()

    -- ^ Mutable arrays     (IOArray in Haskell)
    NewArr :: Integral n => exp n -> exp a                -> CMD exp (Arr (exp a))
    GetArr :: Integral n => exp n          -> Arr (exp a) -> CMD exp (exp a)
    SetArr :: Integral n => exp n -> exp a -> Arr (exp a) -> CMD exp ()

    -- no new var. is assigned.
    UnsafeGetRef :: Ref (exp a) -> CMD exp (exp a)

    -- ^ Control structures | Todo: Move to seperate data class
    If    :: exp Bool
          -> Program (CMD exp) ()
          -> Program (CMD exp) ()
          -> CMD exp ()
    While :: Program (CMD exp) (exp Bool)
          -> Program (CMD exp) ()
          -> CMD exp ()
    Break :: CMD exp ()

    -- ^ Misc.
    Printf  :: Show a => String -> exp a -> CMD exp ()
    GetTime :: CMD exp (exp Double)

-- |
data Handle
    = HandleComp String
    | HandleEval IO.Handle
  deriving Typeable

-- |
data Ref a
    = RefComp String
    | RefEval (IORef a)
  deriving Typeable

-- |
data Arr a
    = ArrComp String
    | ArrEval (IOArray Int a)
  deriving Typeable

--------------------------------------------------------------------------------
-- ** User Interface

--------------------------------------------------------------------------------
-- *** File Handling

open  :: FilePath -> ProgramT (CMD exp) m Handle
open   = singleton . Open

close :: Handle -> ProgramT (CMD exp) m ()
close  = singleton . Close

fput  :: Handle -> exp Float -> ProgramT (CMD exp) m ()
fput p = singleton . Put p

fget  :: Handle -> ProgramT (CMD exp) m (exp Float)
fget   = singleton . Get

feof  :: Handle -> ProgramT (CMD exp) m (exp Bool)
feof   = singleton . Eof

--------------------------------------------------------------------------------
-- *** Variables

newRef        :: exp a -> ProgramT (CMD exp) m (Ref (exp a))
newRef e      = singleton (NewRef e)

getRef        :: Ref (exp a) -> ProgramT (CMD exp) m (exp a)
getRef r      = singleton (GetRef r)

setRef        :: Ref (exp a) -> exp a -> ProgramT (CMD exp) m ()
setRef r      = singleton . SetRef r

--------------------------------------------------------------------------------
-- *** Arrays

newArr :: Integral n => exp n -> exp a -> ProgramT (CMD exp) m (Arr (exp a))
newArr n = singleton . NewArr n

getArr :: Integral n => exp n -> Arr (exp a) -> ProgramT (CMD exp) m (exp a)
getArr n = singleton . GetArr n

setArr :: Integral n => exp n -> exp a -> Arr (exp a) -> ProgramT (CMD exp) m ()
setArr n a = singleton . SetArr n a

----------------------------------------
-- Unsafe

-- | Like 'getRef' but assumes that the reference will not be modified later
--   in the program
unsafeGetRef ::  Ref (exp a) -> ProgramT (CMD exp) m (exp a)
unsafeGetRef = singleton . UnsafeGetRef
  -- TODO: It would be possible to make a conservative analysis to find out if
  --       uses of `unsafeGetRef` are safe. Even better, the compiler could
  --       automatically treat `getRef` as `unsafeGetRef` whenever possible.

--------------------------------------------------------------------------------
-- **

iff :: exp Bool
    -> Program (CMD exp) ()
    -> Program (CMD exp) ()
    -> Program (CMD exp) ()
iff b t f = singleton $ If b t f

while :: Program (CMD exp) (exp Bool)
      -> Program (CMD exp) ()
      -> Program (CMD exp) ()
while b t = singleton $ While b t

break :: Program (CMD exp) ()
break = singleton Break

printf :: Show a => String -> exp a -> Program (CMD exp) ()
printf format = singleton . Printf format

getTime :: Program (CMD exp) (exp Double)
getTime = singleton GetTime

--------------------------------------------------------------------------------
-- * Constructs
--------------------------------------------------------------------------------

-- |
data Construct cmd a
  where
    Function :: String -> Program cmd () -> Construct cmd ()

--------------------------------------------------------------------------------
-- ** User Interface

mkFunction :: String -> Program cmd () -> Program (Construct cmd) ()
mkFunction fun body = singleton $ Function fun body
