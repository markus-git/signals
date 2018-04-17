{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler.Linker.Names where

import Signal.Core.Witness
import qualified Signal.Core as S (Core (..))

import Control.Monad.Identity

import Data.Hashable
import Data.Proxy

-- observable-sharing
import qualified Data.Ref.Map as R (Name)

--------------------------------------------------------------------------------
-- * Distributing Names over a wire.
--------------------------------------------------------------------------------

-- | Sub-names over the individual parts of a name.
data Name a
  where
    Name :: R.Name (S.Core sym exp pred a)      -> Name (S.Core sym exp pred a)
    Fst  :: Name   (S.Core sym exp pred (a, b)) -> Name (S.Core sym exp pred a)
    Snd  :: Name   (S.Core sym exp pred (a, b)) -> Name (S.Core sym exp pred b)

instance Hashable (Name a)
  where
    hashWithSalt s (Name n) = s `hashWithSalt` n
    hashWithSalt s (Fst  l) = s `hashWithSalt` (0 :: Int) `hashWithSalt` l
    hashWithSalt s (Snd  r) = s `hashWithSalt` (1 :: Int) `hashWithSalt` r

--------------------------------------------------------------------------------

-- | A bundle is a collection of names, one for each result of a core construct.
data Bundle a
  where
    -- ^ A single name.
    One  :: Name   (S.Core sym exp pred a)
         -> Bundle (S.Core sym exp pred a)
    -- ^ A pair of names.
    Pair :: Bundle (S.Core sym exp pred a)
         -> Bundle (S.Core sym exp pred b)
         -> Bundle (S.Core sym exp pred (a, b))

-- | Creates a bundle of names from a single name.
bundle :: forall sym exp pred a . Tuple pred a
  => R.Name (S.Core sym exp pred a)
  -> Bundle (S.Core sym exp pred a)
bundle n = go (witness :: TupleRep pred a) (Name n)
  where
    go :: TupleRep pred b
       -> Name   (S.Core sym exp pred b)
       -> Bundle (S.Core sym exp pred b)
    go (Single)    name = One name
    go (Tuple l r) name = Pair (go l $ Fst name) (go r $ Snd name)

--------------------------------------------------------------------------------
