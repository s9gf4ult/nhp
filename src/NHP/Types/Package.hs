module NHP.Types.Package where

import           NHP.Imports

-- | Maybe just Text inside.
newtype PackageId = PackageId
  { unPackageId :: Text }
  deriving (Show, Eq, Ord, IsString)


-- | Baked package with resolved paths and dependencies built in. Can
-- not be changed, can be only resolved from package bucket. Have link
-- to the original package and bucket, so you always can reresolve it.
data Package = Package
  { packageId    :: PackageId
  , derivation   :: Derivation
  , dependencies :: [Package]
  } deriving (Generic)

instance HasField "outputs" Package Package (Map Text DerivationOutput) (Map Text DerivationOutput) where
  field = field @"derivation" . field @"outputs"
