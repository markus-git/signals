{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Signal.Core where

import Signal.Core.Witness
import Control.Monad.Identity (Identity)

import Data.Bits
import Data.Constraint (Constraint)
import Data.Dynamic (Dynamic)
import Data.Typeable (Typeable, Proxy (..))
import Data.Unique

-- observable-sharing
import qualified Data.Ref as Ref
import Data.Ref (Ref)

import Prelude hiding (Left, Right, repeat, map, zip, zipWith, fst, snd)
import qualified Prelude as P

--------------------------------------------------------------------------------
-- * Signals.
--------------------------------------------------------------------------------

-- | Distribute the expression type 'exp' over type 'a'.
type family Expr exp a
  where
    Expr exp (Identity a) = exp a
    Expr exp (a, b) = (Expr exp a, Expr exp b)

--------------------------------------------------------------------------------

-- | Core signal operations.
data Core sig exp pred a
  where
    -- | Constant value.
    Val :: pred a => exp a -> Core sig exp pred (Identity a)

    -- | Promote a function over expressions to signals.
    Map :: (Tuple pred a, Tuple pred b)
        => (Expr exp a -> Expr exp b)
        -> sig exp pred a
        -> Core sig exp pred b

    -- | Join two signals.
    Pair :: (Tuple pred a, Tuple pred b)
         => sig exp pred a
         -> sig exp pred b
         -> Core sig exp pred (a, b)

    -- | Pick out the left-most signal of a pair.
    Fst :: (Tuple pred a, Tuple pred b)
        => sig exp pred (a, b)
        -> Core sig exp pred a

    -- | Pick out the right-most signal of a pair.
    Snd :: (Tuple pred a, Tuple pred b)
        => sig exp pred (a, b)
        -> Core sig exp pred b

    -- | Introduce a unit delay.
    Delay :: pred a => exp a
          -> sig exp pred (Identity a)
          -> Core sig exp pred (Identity a)

    -- | Hole, used during reification.
    Var :: Tuple pred a => Dynamic -> Core sig exp pred a

-- | A symbol is a reference to a core signal construct.
newtype Symbol exp pred a = Symbol { runSym :: Ref (Core Symbol exp pred a) }

-- | Signal wrapper for symbols.
newtype Signal exp pred a = Signal { runSignal :: Symbol exp pred a }

--------------------------------------------------------------------------------

-- | Wrap a core construct in a unique reference.
ref :: Core Symbol exp pred a -> Symbol exp pred a
ref = Symbol . Ref.ref

-- | Unwrap a reference core construct from a reference.
deref :: Symbol exp pred a -> Core Symbol exp pred a
deref (Symbol s) = Ref.deref s

-- | Construct a signal from a core construct.
signal :: Core Symbol exp pred a -> Signal exp pred a
signal = Signal . ref

-- | Unwrap a signal to access its topmost core construct.
symbol :: Signal exp pred a -> Symbol exp pred a
symbol (Signal s) = s

--------------------------------------------------------------------------------

-- | Repeat a single value.
repeat :: pred a => exp a -> Signal exp pred (Identity a)
repeat = signal . Val

-- | Map a function over a signal.
map :: (Tuple pred a, Tuple pred b)
  => (Expr exp a -> Expr exp b)
  -> Signal exp pred a
  -> Signal exp pred b
map f = signal . Map f . symbol

-- | Join two signals.
zip :: (Tuple pred a, Tuple pred b)
  => Signal exp pred a
  -> Signal exp pred b
  -> Signal exp pred (a, b)
zip a b = signal $ Pair (symbol a) (symbol b)

-- | Gets the left-most signal in a pair.
fst :: (Tuple pred a, Tuple pred b)
  => Signal exp pred (a, b)
  -> Signal exp pred a
fst = signal . Fst . symbol

-- | Gets the right-most signal in a pair.
snd :: (Tuple pred a, Tuple pred b)
  => Signal exp pred (a, b)
  -> Signal exp pred b
snd = signal . Snd . symbol

-- | Delay a signal by one unit.
delay :: pred a
  => exp a
  -> Signal exp pred (Identity a)
  -> Signal exp pred (Identity a)
delay e = signal . Delay e . symbol

-- | Creates a hole, useful during reification.
var :: Tuple pred a => Dynamic -> Signal exp pred a
var = signal . Var

--------------------------------------------------------------------------------
-- the end.
