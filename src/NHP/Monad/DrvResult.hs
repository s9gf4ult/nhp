module NHP.Monad.DrvResult where

import           Data.Map.Strict   as M
import           NHP.Imports
import           NHP.Monad.Backend
import           NHP.Monad.Type
import           NHP.Script
import           NHP.Types


appendScript :: (Monad f) => Script -> DerivationM f ()
appendScript s = DerivationM $ do
  modify $ field @"script" %~ (<> s)

prependScript :: (Monad f) => Script -> DerivationM f ()
prependScript s = DerivationM $ do
  modify $ field @"script" %~ (s <>)

-- | Sets output.
setOutput :: (Monad f, HasCallStack) => OutputId -> Output -> DerivationM f ()
setOutput oid output = DerivationM $ do
  preuse (field @"outputs" . ix oid) >>= \case
    Nothing -> modify $ field @"outputs" %~ (M.insert oid output)
    Just oldOid
      | oldOid == output -> return ()
      | otherwise        -> do
          f <- asks _failDerivation
          lift $ f $ OutputAlreadyExists
            $ OutputExistsError oid oldOid output

setLicense :: (Monad f) => Maybe License -> DerivationM f ()
setLicense license = DerivationM $ do
  modify $ field @"license" .~ license

listenScript :: (Monad f) => DerivationM f a -> DerivationM f (a, Script)
listenScript ma = do
  oldS <- DerivationM $ use (field @"script")
    <* modify (field @"script" .~ mempty)
  a <- ma -- Modify the empty script
  maScript <- DerivationM $ use (field @"script")
    <* modify (field @"script" .~ oldS)
  return (a, maScript)

getPackageOutput :: (Monad f, HasCallStack) => Package -> OutputId -> DerivationM f Path
getPackageOutput pkg out = case pkg ^? field @"derivation" . field @"outputs" . ix (unOutputId out) of
  Nothing  -> failDerivation $ OutputNotFound (pkg ^. field @"packageId") out
  Just out -> return $ out ^. field @"path" . re _Path

packageFile :: (Monad f, HasCallStack) => PackageFile -> DerivationM f Path
packageFile (PackageFile pkgId out path) = do
  pkg <- evalPackage pkgId
  outPath <- getPackageOutput pkg out
  return $ outPath </> path
