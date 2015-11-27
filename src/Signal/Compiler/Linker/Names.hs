{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Signal.Compiler.Linker.Names where

import Signal.Core  (S)
import Signal.Core.Witness

import Control.Monad.Identity
import Data.Hashable
import Data.Ref.Map (Name)

--------------------------------------------------------------------------------
-- * Distributing
--------------------------------------------------------------------------------

-- | ...
type family Distributed (node :: * -> *) a where
  Distributed node (S sym i (Identity a)) = node (S sym i (Identity a))
  Distributed node (S sym i (a, b))       = ( Distributed node (S sym i a)
                                            , Distributed node (S sym i b))

-- | ...
data Named a
  where
    Named  :: Name  (S sym i a)      -> Named (S sym i a)
    Lefty  :: Named (S sym i (a, b)) -> Named (S sym i a)
    Righty :: Named (S sym i (a, b)) -> Named (S sym i b)
    Other  :: Named (S sym i a)      -> Named (S sym i a)

instance Hashable (Named a)
  where
    hashWithSalt s (Named  n) = s `hashWithSalt` n
    hashWithSalt s (Lefty  l) = s `hashWithSalt` (0 :: Int) `hashWithSalt` l
    hashWithSalt s (Righty r) = s `hashWithSalt` (1 :: Int) `hashWithSalt` r
    hashWithSalt s (Other  n) = s `hashWithSalt` (2 :: Int) `hashWithSalt` n

--------------------------------------------------------------------------------
-- ** Naming wires, i.e distributing one wire's name over its subwires

-- | Shorthand for distributed names
type Names a = Distributed Named a

-- | Takes a composite name and creates unique names for each part
name :: forall sym i a. Witness i a => Name (S sym i a) -> Names (S sym i a)
name n = go (witness :: Wit i a) (Named n)
  where
    go :: Wit i x -> Named (S sym i x) -> Names (S sym i x)
    go (WE)     n = n
    go (WP l r) n = (go l (Lefty n), go r (Righty n))

-- | ...
other :: Names (S sym i (Identity a)) -> Names (S sym i (Identity a))
other = Other

--------------------------------------------------------------------------------
