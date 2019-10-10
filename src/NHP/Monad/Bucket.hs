module NHP.Monad.Bucket where

import           Data.Map.Strict as M
import           NHP.Imports
import           NHP.Monad.Types
import           NHP.Script
import           NHP.Types

newPackage
  :: (HasCallStack, DefScript script)
  => PackageId
  -> (HasCallStack => DerivationM script f ())
  -> BucketM f ()
newPackage pkgId drv = do
  preuse (field @"bucket" . ix pkgId) >>= \case
    Nothing -> modify $ field @"bucket" %~
      (M.insert pkgId (BucketDerivation $ SomeDerivation drv))
    Just _ -> throwWithStack $ PackageAlreadyAdded pkgId

newClosure
  :: (HasCallStack, DefScript script)
  => PackageId
  -> (HasCallStack => BucketM f ())
  -> (HasCallStack => DerivationM script f ())
  -> BucketM f ()
newClosure pkgId closureScope drv = do
  preuse (field @"bucket" . ix pkgId) >>= \case
    Just _  -> throwWithStack $ PackageAlreadyAdded pkgId
    Nothing -> do
      ((), st) <- listenState $ do
        put emptyBucketState
        closureScope
      let bucket = st ^. field @"bucket"
      modify $ field @"bucket" %~
        (M.insert pkgId (BucketClosure (SomeDerivation drv) bucket))

-- | Expects the closure to be already presented in the bucket
patchClosure
  :: HasCallStack
  => PackageId
  -> (HasCallStack => BucketM f ())
  -> BucketM f ()
patchClosure pkgId closure = do
  preuse (field @"bucket" . ix pkgId) >>= \case
    Nothing -> throwWithStack $ PackageAbsent pkgId
