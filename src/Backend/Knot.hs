{-# LANGUAGE RecursiveDo #-}

module Backend.Knot (
    Knot
  , Solver
  , tie
  )
where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Fix

--------------------------------------------------------------------------------
-- * Knot Monad
--------------------------------------------------------------------------------

-- | Knot monad transformer
type Knot resolution constraint m = ReaderT resolution (WriterT [constraint] m)

-- | Resolve linking constraints
type Solver resolution constraint = [constraint] -> resolution

-- | Tie the knot using @solve@ to resolve any constraints
tie :: MonadFix m => Solver resolution constraint -> Knot resolution constraint m a -> m (a, resolution)
tie solve knot =
  mdo (a, constraints) <- runWriterT $ runReaderT knot solution
      let solution = solve constraints
      return (a, solution)

--------------------------------------------------------------------------------
