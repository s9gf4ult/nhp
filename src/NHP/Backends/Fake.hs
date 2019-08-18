-- | Fake backend for deriving. Does not work with real nix-daemon but
-- generates derivations purely.

module NHP.Backends.Fake where

import           Data.Map.Strict as M
import           Data.Set        as S
import           Filesystem.Path as F
import           NHP.Bucket
import           NHP.Error
import           NHP.Imports
import           NHP.Monad
import           NHP.Script
import           NHP.Types

data EvalState

data EvalError
  = NoPackageFound PackageId

newtype FakeBackend a = FakeBackend
  { unFakeBackend :: ExceptT (WithCallStack EvalError) (State EvalState) a
  } deriving (Functor, Applicative, Monad, MonadError (WithCallStack EvalError))

type FakeBucket = PackageBucket FakeBackend

evalDerivation
  :: HasCallStack
  => FakeBucket
  -> PackageId
  -> FakeBackend Package
evalDerivation bucket pkgId = case bucket ^? field @"packages" . ix pkgId of
  Nothing  -> throwWithStack $ NoPackageFound pkgId
  Just drvM -> do
    let backend = (error "FIXME: not implemented")
    ((builderPath, builderArgs, scriptPath), result) <- runDerivationM backend $ do
      ((), script) <- listenScript drvM
      let ScriptResult interp scriptBin genArgs = runScript script
      builder <- packageFile interp
      scriptPath <- storeBinary scriptBin
      return (builder, genArgs scriptPath, scriptPath)
    let
      deps = (error "FIXME: not implemented")
      defaultPlatform = platformText $ bucket ^. field @"platform"
      derivation = Derivation
        { outputs = result ^. field @"outputs"
          . to (M.fromList . fmap toOutput . M.toList)
        , inputDrvs = getInputDrvs deps
        , inputSrcs = S.insert (scriptPath ^. _Path) $ getInputSrcs deps
        , platform  = maybe defaultPlatform platformText
          $ result ^. field @"platform"
        , builder = pathText builderPath
        , args = builderArgs
        , env = result ^. field @"env"
        }
      package = Package
        { packageId = pkgId
        , derivation = derivation
        , dependencies = deps
        }
    return package
  where
    toOutput (outId, output) = (unOutputId outId, drv)
      where
        drv = DerivationOutput
          { path = error "FIXME: precalculate path of the output before the derivation"
          , hashAlgo = case output of
              FixedHashOutput _ -> "sha256"
              _                 -> ""
          , hash = case output of
              FixedHashOutput sha -> sha256Text sha
              _                   -> ""
          }

getInputDrvs :: [Package] -> Map F.FilePath (Set Text)
getInputDrvs = error "FIXME: getInputDrvs not implemented"

getInputSrcs :: [Package] -> Set F.FilePath
getInputSrcs = error "FIXME: getInputSrcs not implemented"
