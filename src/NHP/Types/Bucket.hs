module NHP.Types.Bucket where

import           NHP.Import

data PackageBucket f = PackageBucket
  { packages :: Map PackageId (DerivationM f ())
  }

-- | Adds package or fails if package is already exists
addPackage
  :: PackageId
  -> DerivationM f ()
  -> PackageBucket f
  -> Either Text (PackageBucket f)
addPackage = error "FIXME: addPackage not implemented"
