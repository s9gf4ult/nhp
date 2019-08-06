module NHP.Monad where

import           NHP.Types

data DState

newtype DerivationM f a = DerivationM
  { runDerivation
    :: (PackageId -> f Package)
    -> DState
    -> f (a, Script, DState)
  }

instance (Functor f) => Functor (DerivationM f) where
  fmap f (DerivationM g) = DerivationM $ \p s -> go <$> g p s
    where
      go (a, s, d) = (f a, s, d)

instance (Monad f) => Applicative (DerivationM f) where
  pure a = DerivationM $ \_p s -> pure (a, mempty, s)
  (DerivationM mf) <*> (DerivationM ma) = DerivationM $ \p d -> do
    (f, s1, d1) <- mf p d
    (a, s2, d2) <- ma p d1
    return (f a, s1 <> s2, d2)

-- instance
