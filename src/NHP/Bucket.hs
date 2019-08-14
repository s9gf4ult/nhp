module NHP.Bucket where

import           NHP.Imports
import           NHP.Monad
import           NHP.Types

data PackageBucket f = PackageBucket
  { packages :: Map PackageId (DerivationM f ())
  }

emptyBucket :: PackageBucket f
emptyBucket = PackageBucket mempty

-- | Adds package or fails if package is already exists
addPackage
  :: PackageId
  -> DerivationM f ()
  -> PackageBucket f
  -> Either Text (PackageBucket f)
addPackage = error "FIXME: addPackage not implemented"

evalDerivation :: (Monad f) => PackageId -> PackageBucket f -> f Package
evalDerivation = error "FIXME: evalDerivation not implemented"
