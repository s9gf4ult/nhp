module NHP.Bucket where

import           NHP.Imports
import           NHP.Monad
import           NHP.Types

data PackageBucket f = PackageBucket
  { packages :: Map PackageId (DerivationM f ())
  , platform :: Platform
  -- ^ Default platform for all packages in the bucket. (If platform
  -- is not specified by the derivation)
  } deriving (Generic)

emptyBucket :: Platform -> PackageBucket f
emptyBucket = PackageBucket mempty

-- | Adds package or fails if package is already exists
addPackage
  :: PackageId
  -> DerivationM f ()
  -> PackageBucket f
  -> Either Text (PackageBucket f)
addPackage = error "FIXME: addPackage not implemented"
