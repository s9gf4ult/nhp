module NHP.Monad.Derivation.Backend where

import           Filesystem.Path           as F
import           NHP.Imports
import           NHP.Monad.Derivation.Type
import           NHP.Types


evalDerivation
  :: (Monad f, HasCallStack)
  => DerivationM f ()
  -> [OutputId]
  -> DerivationM f (Map OutputId Path)
evalDerivation = error
  "FIXME: every derivation must have id, so this thing must be removed"

-- | Set dependency on given package
evalPackageOutput
  :: (Monad f, HasCallStack)
  => PackageId
  -> OutputId
  -> DerivationM f Path
evalPackageOutput pkgid output = do
  g <- asks _evalPackageOutput
  lift $ g pkgid output

failDerivation :: (Monad f, HasCallStack) => DerivationFail -> DerivationM f a
failDerivation t = do
  f <- asks _failDerivation
  lift $ f t

storePath :: (Monad f, HasCallStack) => F.FilePath -> DerivationM f Path
storePath fp = do
  s <- asks _storePath
  lift $ s fp

storeBinary :: (Monad f, HasCallStack) => ByteString -> DerivationM f Path
storeBinary bs = do
  s <- asks _storeBinary
  lift $ s bs
