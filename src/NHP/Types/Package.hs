module NHP.Types.Package where

import           NHP.Imports
import           NHP.Types.Aux

type PackageDeps = Map Package (Set OutputId)

type SrcDeps = Set Path

-- | Baked package with resolved paths and dependencies built in. Can
-- not be changed, can be only resolved from package bucket. Have link
-- to the original package and bucket, so you always can reresolve it.
data Package = Package
  { point          :: PackagePoint
  , derivation     :: Derivation
  , derivationPath :: Path
  , packageDeps    :: PackageDeps
  , srcDeps        :: SrcDeps
  } deriving (Eq, Ord, Generic)
