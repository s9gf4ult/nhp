module NHP.Monad.Derivation.DrvResult where

import           Data.Map.Strict              as M
import           NHP.Imports
import           NHP.Monad.Derivation.Backend
import           NHP.Monad.Derivation.Type
import           NHP.Script
import           NHP.Types


appendScript :: (Monad f) => Script -> DerivationM f ()
appendScript s = DerivationM $ do
  modify $ field @"script" %~ (<> s)

prependScript :: (Monad f) => Script -> DerivationM f ()
prependScript s = DerivationM $ do
  modify $ field @"script" %~ (s <>)

-- | Sets output.
setOutput :: (Monad f, HasCallStack) => OutputId -> Output -> DerivationM f (Exp Path)
setOutput oid output = DerivationM $ do
  preuse (field @"outputs" . ix oid) >>= \case
    Nothing -> do
      modify $ field @"outputs" %~ (M.insert oid output)
      return $ outputVar oid
    Just oldOid
      | oldOid == output -> return $ outputVar oid
      | otherwise        -> do
          f <- asks _failDerivation
          lift $ f $ OutputAlreadyExists
            $ OutputExistsError oid oldOid output

defaultOutput :: (Monad f, HasCallStack) => DerivationM f (Exp Path)
defaultOutput = setOutput def SimpleOutput

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

packageFile :: (Monad f, HasCallStack) => PackageFile -> DerivationM f Path
packageFile (PackageFile pkgId out path) = do
  outPath <- evalPackageOutput pkgId out
  return $ outPath </> path
