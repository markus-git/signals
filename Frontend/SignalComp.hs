module Frontend.SignalComp where

import           Frontend.Stream (Stream(..))
import qualified Frontend.Stream as Str

import           Frontend.Signal (Signal(..))
import qualified Frontend.Signal as Sig

import           Frontend.SignalObsv (TSignal(..), Tree(..), T)
import qualified Frontend.SignalObsv as SigO

import           Data.Map (Map)
import qualified Data.Map as M

import Core (CMD, Ref)
import Frontend.Signal     (Signal(..))
import Frontend.SignalObsv (TSignal(..))

import Control.Monad.Operational
import Data.Foldable (find)
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
type Id   = Unique





linkMap :: [Node] -> Root -> Map Id (Tree Id)
linkMap = undefined
  where
    go :: Node -> Map Id (Tree Id) -> Tree Id
    go (u, n) m
      | Just t <- M.lookup u m = undefined
      | otherwise              = undefined

findNode :: [Node] -> Id -> Maybe Node
findNode nodes i = find ((==) i . fst) nodes

----------------------------------------

fstT :: Tree a -> Tree a
fstT (B l r) = l
fstT (L _)   = error "fstT"

sndT :: Tree a -> Tree a
sndT (B l r) = r
sndT (L _)   = error "sndT"

zipT :: Tree a -> Tree a -> Tree a
zipT = B
