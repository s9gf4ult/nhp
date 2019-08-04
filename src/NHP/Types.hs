module NHP.Types where

-- | Maybe just Text inside.
data PackageId

-- | Baked package with resolved paths and dependencies built in. Can
-- not be changed, can be only resolved from package bucket. Have link
-- to the original package and bucket, so you always can reresolve it.
data Package = Package
  { pId   :: PackageId
  -- , pDerivation :: Derivation
  , pDeps :: [Package]
  }
