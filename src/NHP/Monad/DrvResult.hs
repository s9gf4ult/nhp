module NHP.Monad.DrvResult where

import           NHP.Imports
import           NHP.Monad.Type
import NHP.Monad.Backend
import           NHP.Script
import           NHP.Types
import Data.Map.Strict as M

-- | Some file in the package.
data PackageFile = PackageFile
  { package :: PackageId
  , output :: OutputId
  , path :: Path
  }

data ScriptResult = ScriptResult
  { interpreter :: PackageFile
  -- ^ The interpreter to run the script
  , script :: ByteString
  -- ^ The raw generated script
  }

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

runScript :: Derivation f ScriptResult
runScript = DerivationM $ do
  script <- use $ field @"script"
