module NHP.State where

import           Control.Monad.State.Strict

freezeState :: (MonadState s m) => m a -> m a
freezeState ma = do
  oldS <- get
  ma <* put oldS

-- | Not actually 'listen' but just returns the final state. You can
-- set the initial state inside of the 'ma' action
listenState :: (MonadState s m) => m a -> m (a, s)
listenState ma = freezeState $ do
  a <- ma
  newS <- get
  return (a, newS)
