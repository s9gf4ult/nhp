module NHP.Monad.Backend where

import           NHP.Imports
import           NHP.Monad.Type
import           NHP.Types

evalDerivation :: (Monad f) => DerivationM f a -> DerivationM f (Package, a)
evalDerivation drv = do
  bd <- asks _evalDerivation
  lift $ bd drv

-- | Set dependency on given package
evalPackage :: (Monad f) => PackageId -> DerivationM f Package
evalPackage pkgid = do
  g <- asks _evalPackage
  lift $ g pkgid

failDerivation :: (Monad f, HasCallStack) => DerivationFail -> DerivationM f a
failDerivation t = do
  f <- asks _failDerivation
  lift $ f t

storePath :: (Monad f) => FilePath -> DerivationM f Path
storePath fp = do
  s <- asks _storePath
  lift $ s fp

storeBinary :: (Monad f) => ByteString -> DerivationM f Path
storeBinary bs = do
  s <- asks _storeBinary
  lift $ s bs
