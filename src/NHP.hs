-- | Naive Haskell Packager

module NHP where

import           Data.Set
import           NHP.Types

data Deps = Deps (Set PackageId)

-- | Raw package definition without resolved paths and so on. May be
-- overriden and changed at anly time.
data PackageDef = PackageDef
  { pdId    :: PackageId
  -- , pdBuild :: m Package
  }

runM :: m Package -> (Package, Deps)
runM x = undefined



data PackageBucket

data Env

putDef :: PackageId -> PackageDef -> PackageBucket -> PackageBucket
putDef = undefined

patchDef :: PackageId -> (PackageDef -> PackageDef) -> PackageBucket -> PackageBucket
patchDef x = undefined

resolvePackage :: PackageBucket -> PackageId -> m Package
resolvePackage = undefined

putPkg :: Package -> Env -> Env
putPkg x = undefined
