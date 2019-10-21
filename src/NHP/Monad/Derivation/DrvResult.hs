module NHP.Monad.Derivation.DrvResult where

import           Control.Monad.Trans.RWS.Strict (RWST (..))
import           Data.Map.Strict                as M
import           NHP.Imports
import           NHP.Monad.Types
import           NHP.Script
import           NHP.Types


-- appendScript :: (Monad f) => Script -> DerivationM f ()
-- appendScript s = DerivationM $ do
--   modify $ field @"script" %~ (<> s)

-- prependScript :: (Monad f) => Script -> DerivationM f ()
-- prependScript s = DerivationM $ do
--   modify $ field @"script" %~ (s <>)

-- | Sets output.
setOutput
  :: (Monad f, HasCallStack)
  => OutputId
  -> Output
  -> DerivationM script f (Exp Path)
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

defaultOutput
  :: (Monad f, HasCallStack)
  => DerivationM script f (Exp Path)
defaultOutput = setOutput def SimpleOutput

setLicense
  :: (Monad f)
  => Maybe License
  -> DerivationM script f ()
setLicense license = DerivationM $ do
  modify $ field @"license" .~ license

listenScript
  :: (Monad f, DefScript script)
  => DerivationM script f a
  -> DerivationM script f (a, Script script)
listenScript ma = do
  oldS <- DerivationM $ use (field @"script")
    <* modify (field @"script" .~ def)
  a <- ma -- Modify the empty script
  maScript <- DerivationM $ use (field @"script")
    <* modify (field @"script" .~ oldS)
  return (a, maScript)

packageFile
  :: (Monad f, HasCallStack)
  => PackageFile
  -> DerivationM script f Path
packageFile (PackageFile pkgId out path) = do
  outPath <- evalPackageOutput pkgId out
  return $ outPath </> path

emptyResult :: DefScript script => DrvResult script
emptyResult = DrvResult
  { script   = def
  , outputs  = mempty
  , license  = Nothing
  , platform = Nothing
  , env      = mempty
  }

runDerivationM
  :: (DefScript script, Monad f)
  => DrvMethods f
  -> DerivationM script f a
  -> ResolveM f (a, DrvResult script)
runDerivationM backend drv = dropTup <$> runRWST (unDerivation drv) backend emptyResult
  where
    dropTup (a, b, _) = (a, b)
