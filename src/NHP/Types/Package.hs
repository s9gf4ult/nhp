module NHP.Types.Package where

import           NHP.Imports

-- | Maybe just Text inside.
newtype PackageId = PackageId
  { unPackageId :: Text }
  deriving (Show, Eq, IsString)


-- | Baked package with resolved paths and dependencies built in. Can
-- not be changed, can be only resolved from package bucket. Have link
-- to the original package and bucket, so you always can reresolve it.
data Package = Package
  { packageId    :: PackageId
  , name         :: DerivationName
  , path         :: Path
  , outputs      :: Map OutputId Path
  , dependencies :: [Package]
  } deriving (Generic)
