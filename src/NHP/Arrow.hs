
module NHP.Arrow where

import           Control.Arrow
import           Control.Category
import           Data.Set
import           Data.Text
import           NHP.Types

newtype Script = Script Text
  deriving (Eq, Ord, Semigroup, Monoid)

data Derivation f a b = Derivation
  { builder      :: (PackageId -> f Package) -> a -> f (b, Script)
  -- ^ Script generator. a and b are to combine building
  -- scripts. First argument function calculates package and adds it
  -- as a dependency.
  }

instance (Monad f) => Category (Derivation f) where
  id = Derivation $ \_ a -> pure (a, mempty)
  (.) (Derivation bc) (Derivation ab) = Derivation $ \p a -> do
    (b, s1) <- ab p a
    (c, s2) <- bc p b
    return (c, s1 <> s2)

instance (Monad f) => Arrow (Derivation f) where
  arr f = Derivation $ \_ a -> pure (f a, mempty)
  (Derivation f) *** (Derivation g) = Derivation $ \p (a, aa) ->
    go <$> f p a <*> g p aa
    where
      go (b, s1) (bb, s2) = ((b, bb), s1 <> s2)

example :: Derivation f () ()
example = proc () -> do
  p <- fetchUri "http://yoba"
  src <- unpack -< p
  deps <- myDeps ["yoba", "boba"]
  configure -< (src, deps)
  build -< src
  install -< src
