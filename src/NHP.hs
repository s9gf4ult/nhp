-- | Naive Haskell Packager

module NHP where

data Deps = Deps (Set PackageId)

-- | Raw package definition without resolved paths and so on. May be
-- overriden and changed at anly time.
data PackageDef = PackageDef
  { pdId    :: PackageId
  , pdBuild :: m Package
  }

runM :: m Package -> (Package, Deps)
runM x = undefined

-- | Baked package with resolved paths and dependencies built in. Can
-- not be changed, can be only resolved from package bucket. Have link
-- to the original package and bucket, so you always can reresolve it.
data Package = Package
  { pId         :: PackageId
  , pDerivation :: Derivation
  , pDeps       :: [Package]
  }

-- | Maybe just Text inside.
data PackageId


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
