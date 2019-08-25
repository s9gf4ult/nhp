module NHP.Monad.Types.Error where

import           NHP.Imports
import           NHP.Types

data ResolveError
  = NoPackageFound PackageId
  | NoPackageOutput PackageId OutputId
  | DerivationFailed DerivationError
  | CircularDependencies [PackageId]
  | PackageStackIsEmpty
  | EvalAssertionFailed Text
  deriving (Ord, Eq, Generic)

data DerivationError
  = PlatformNotSupported Text
  -- ^ This derivation is will not work on this platform
  | PackageBroken Text
  -- ^ The program will not work correctly or build script will fail
  -- to build the derivation.
  | OutputAlreadyExists OutputExistsError
  | NoOutputFound OutputId
  | SomeFail Text
  -- ^ Eval time failure
  deriving (Ord, Eq, Generic)

-- | Multiple output declarations.
data OutputExistsError = OutputExistsError
  { outputId  :: OutputId
  , oldOutput :: Output
  , newOutput :: Output
  } deriving (Eq, Ord, Generic)

data BucketError
  = PackageAlreadyAdded PackageId
