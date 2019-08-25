module NHP.Monad.Bucket where

import           Data.Map.Strict as M
import           NHP.Imports
import           NHP.Monad.Types
import           NHP.Types

newPackage :: HasCallStack => PackageId -> DerivationM f () -> BucketM f ()
newPackage pkgId drv = do
  preuse (field @"bucket" . ix pkgId) >>= \case
    Nothing -> modify $ field @"bucket" %~ (M.insert pkgId drv)
    Just _ -> throwWithStack $ PackageAlreadyAdded pkgId
