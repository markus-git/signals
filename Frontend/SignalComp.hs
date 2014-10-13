module Frontend.SignalComp where

import           Frontend.Stream (Stream(..))
import qualified Frontend.Stream as Str

import           Frontend.Signal (Signal (..))
import qualified Frontend.Signal as Sig

import           Frontend.SignalObsv (TSignal (..))
import qualified Frontend.SignalObsv as SigO

import           Data.Map (Map)
import qualified Data.Map as M

import Core (CMD)
import Frontend.Signal     (Signal(..))
import Frontend.SignalObsv (TSignal(..))

import Control.Monad.Operational
import Data.Typeable
import Data.Reify

--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

compile :: (Signal a -> Signal b) -> IO (Program (CMD exp) a -> Program (CMD exp) b)
compile = undefined

--------------------------------------------------------------------------------
-- **

-- |
type Node = (Unique, TSignal Unique)

-- |
type Root = Unique

-- |
type Id = Unique


linker :: [Node] -> Root -> Map Id [Id]
linker = undefined
