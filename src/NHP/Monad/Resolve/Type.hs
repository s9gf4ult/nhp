-- | Fake backend for deriving. Does not work with real nix-daemon but
-- generates derivations purely.

module NHP.Monad.Resolve.Type where

import           Control.Monad.Trans.RWS.Strict (RWST (..))
import           Data.Map.Strict                as M
import           Filesystem.Path                as F
import           NHP.Bucket
import           NHP.Error
import           NHP.Imports
import           NHP.Monad.Derivation
import           NHP.Types
import           Prelude                        as P

data CurrentPackage = CurrentPackage
  { packageId   :: PackageId
  , packageDeps :: PackageDeps
  , srcDeps     :: SrcDeps
  } deriving (Eq, Ord, Generic)

data EvalState = EvalState
  { cache :: Map PackageId Package
  -- ^ Already calculated packages
  , stack :: [CurrentPackage]
  } deriving (Generic)

data NixBackend f = NixBackend
  { _storeAdd       :: HasCallStack => F.FilePath -> f Path
  -- ^ Store derivation or other path in the Nix store
  , _storeAddBinary :: HasCallStack => ByteString -> f Path
  -- ^ Store binary data in the store and return the path
  }

data EvalError
  = NoPackageFound PackageId
  | NoPackageOutput PackageId OutputId
  | DerivationFailed DerivationFail
  | CircularDependencies [PackageId]
  | PackageStackIsEmpty
  | EvalAssertionFailed Text
  deriving (Ord, Eq, Generic)

emptyStackFailure :: (Monad f, HasCallStack) => FakeBackend f a
emptyStackFailure = throwWithStack
  $ EvalAssertionFailed "Current evaluating package is empty"

newtype FakeBackend f a = FakeBackend
  { unFakeBackend :: ExceptT (WithCallStack EvalError) (RWST (NixBackend f) () EvalState f) a
  } deriving
  ( Functor, Applicative, Monad, MonadError (WithCallStack EvalError)
  , MonadState EvalState, MonadReader (NixBackend f) )

instance MonadTrans FakeBackend where
  lift ma = FakeBackend $ lift $ lift ma

type FakeBucket f = PackageBucket (FakeBackend f)
