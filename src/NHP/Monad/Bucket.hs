module NHP.Monad.Bucket where

import           Data.Map.Strict as M
import           NHP.Imports
import           NHP.Monad.Types
import           NHP.Types

newPackage
  :: HasCallStack
  => PackageId
  -> (HasCallStack => DerivationM f ())
  -> BucketM f ()
newPackage pkgId drv = do
  preuse (field @"bucket" . ix pkgId) >>= \case
    Nothing -> modify $ field @"bucket" %~ (M.insert pkgId (BucketDerivation drv))
    Just _ -> throwWithStack $ PackageAlreadyAdded pkgId

newClosure
  :: HasCallStack
  => PackageId
  -> (HasCallStack => BucketM f ())
  -> (HasCallStack => DerivationM f ())
  -> BucketM f ()
newClosure pkgId closureScope drv = do
  preuse (field @"bucket" . ix pkgId) >>= \case
    Just _  -> throwWithStack $ PackageAlreadyAdded pkgId
    Nothing -> do
      ((), st) <- listenState $ do
        put emptyBucketState
        closureScope
      let bucket = st ^. field @"bucket"
      modify $ field @"bucket" %~ (M.insert pkgId (BucketClosure drv bucket))

-- | Expects the closure to be already presented in the bucket
patchClosure
  :: HasCallStack
  => PackageId
  -> (HasCallStack => BucketM f ())
  -> BucketM f ()
patchClosure pkgId closure = do
  preuse (field @"bucket" . ix pkgId) >>= \case
    Nothing -> throwWithStack $ PackageAbsent pkgId
