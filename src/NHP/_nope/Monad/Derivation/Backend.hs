module NHP.Monad.Derivation.Backend where

import           Filesystem.Path as F
import           NHP.Imports
import           NHP.Monad.Types
import           NHP.Types

-- | Set dependency on given package
evalPackageOutput
  :: (Monad f, HasCallStack)
  => PackageId
  -> OutputId
  -> DerivationM script f Path
evalPackageOutput pkgid output = do
  g <- asks _evalPackageOutput
  DerivationM $ lift $ g pkgid output

failDerivation :: (Monad f, HasCallStack) => DerivationError -> DerivationM script f a
failDerivation t = do
  f <- asks _failDerivation
  DerivationM $ lift $ f t

storePath :: (Monad f, HasCallStack) => F.FilePath -> DerivationM script f Path
storePath fp = do
  s <- asks _storePath
  DerivationM $ lift $ s fp

storeBinary :: (Monad f, HasCallStack) => ByteString -> DerivationM script f Path
storeBinary bs = do
  s <- asks _storeBinary
  DerivationM $ lift $ s bs
