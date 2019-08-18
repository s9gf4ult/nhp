module NHP.Types.Package where

import           NHP.Imports
import           NHP.Types.Aux

-- | Baked package with resolved paths and dependencies built in. Can
-- not be changed, can be only resolved from package bucket. Have link
-- to the original package and bucket, so you always can reresolve it.
data Package = Package
  { packageId    :: PackageId
  , derivation   :: Derivation
  , dependencies :: [Package]
  } deriving (Generic)
