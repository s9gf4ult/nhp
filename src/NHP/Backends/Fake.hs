-- | Fake backend for deriving. Does not work with real nix-daemon but
-- generates derivations purely.

module NHP.Backends.Fake where

import NHP.Bucket
import Data.Map.Strict as M
import NHP.Error
import NHP.Imports
import Filesystem.Path as F
import NHP.Monad
import NHP.Types

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
    ((), result) <- runDerivationM drvM backend
    let
      deps = (error "FIXME: not implemented")
      defaultPlatform = platformText $ bucket ^. field @"platform"
      derivation = Derivation
        { outputs = result ^. field @"outputs"
          . to (M.fromList . fmap toOutput . M.toList)
        , inputDrvs = getInputDrvs deps
        , inputSrcs = getInputSrcs deps
        , platform  = maybe defaultPlatform platformText
          $ result ^. field @"platform"
        , builder = builderPath
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
          { path = error "FIXME: precalculate path before the derivation"
          , hashAlgo = case output of
              FixedHashOutput _  -> "sha256"
              _                  -> ""
          , hash = case output of
              FixedHashOutput sha -> sha256Text sha
              _                   -> ""
          }

getInputDrvs :: [Package] -> Map F.FilePath (Set Text)
getInputDrvs = error "FIXME: getInputDrvs not implemented"

getInputSrcs :: [Package] -> Set F.FilePath
getInputSrcs = error "FIXME: getInputSrcs not implemented"
