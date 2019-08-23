-- | Fake backend for deriving. Does not work with real nix-daemon but
-- generates derivations purely.

module NHP.Backends.Fake where

import           Control.Monad.Trans.RWS.Strict (RWST (..))
import           Data.Map.Strict                as M
import           Data.Set                       as S
import           Data.Text.Lazy.Builder         as TB
import           Data.Text.Lazy.Encoding        as TE
import           Filesystem.Path                as F
import           NHP.Bucket
import           NHP.Error
import           NHP.Imports
import           NHP.Monad
import           NHP.Script
import           NHP.State
import           NHP.Types
import           Prelude                        as P

data CurrentPackage = CurrentPackage
  { packageId   :: PackageId
  , packageDeps :: PackageDeps
  , srcDeps     :: SrcDeps
  } deriving (Generic)

currentPackage :: PackageId -> CurrentPackage
currentPackage pkgId = CurrentPackage pkgId M.empty S.empty

setPackageDependency :: (Monad f, HasCallStack) => Package -> OutputId -> FakeBackend f ()
setPackageDependency pkg outId = do
  cp <- stackHead
  let addDep = M.insertWith S.union pkg (S.singleton outId)
  modify $ (field @"stack" . _head . field @"packageDeps") %~ addDep

stackHead :: (Monad f, HasCallStack) => FakeBackend f CurrentPackage
stackHead = preuse (field @"stack" . _head) >>= \case
  Nothing -> throwWithStack
    $ EvalAssertionFailed "Current evaluating package is empty"
  Just cp -> return cp

setPathDependency :: (Monad f, HasCallStack) => Path -> FakeBackend f ()
setPathDependency path = do
  cp <- stackHead
  let addDep = S.insert path
  modify $ (field @"stack" . _head . field @"srcDeps") %~ addDep

data EvalState = EvalState
  { cache :: Map PackageId Package
  -- ^ Already calculated packages
  , stack :: [CurrentPackage]
  } deriving (Generic)

data NixBackend f = NixBackend
  { _storeAdd       :: P.FilePath -> f Path
  -- ^ Store derivation or other path in the Nix store
  , _storeAddBinary :: ByteString -> f Path
  -- ^ Store binary data in the store and return the path
  }

data EvalError
  = NoPackageFound PackageId
  | NoPackageOutput PackageId OutputId
  | DerivationFailed DerivationFail
  | CircularDependencies [PackageId]
  | EvalAssertionFailed Text
  deriving (Ord, Eq, Generic)

newtype FakeBackend f a = FakeBackend
  { unFakeBackend :: ExceptT (WithCallStack EvalError) (RWST (NixBackend f) () EvalState f) a
  } deriving
  ( Functor, Applicative, Monad, MonadError (WithCallStack EvalError)
  , MonadState EvalState, MonadReader (NixBackend f) )

instance MonadTrans FakeBackend where
  lift ma = FakeBackend $ lift $ lift ma

type FakeBucket f = PackageBucket (FakeBackend f)

nixStoreAdd :: (Monad f, HasCallStack) => F.FilePath -> FakeBackend f Path
nixStoreAdd = error "FIXME: nixStoreAdd not implemented"

nixStoreAddBinary :: (Monad f, HasCallStack) => ByteString -> FakeBackend f Path
nixStoreAddBinary = error "FIXME: storeAddBinary not implemented"

withPackage :: (Monad f) => PackageId -> FakeBackend f a -> FakeBackend f a
withPackage pkgId ma = do
  (a, newS) <- listenState $ do
    modify $ field @"stack" %~ ((currentPackage pkgId) :)
    ma
  modify $ field @"cache" .~ (newS ^. field @"cache")
  return a

-- | Check that there is no recursion in the package stack.
checkNoRecursion :: (Monad f) => FakeBackend f ()
checkNoRecursion = error "FIXME: checkNoRecursion not implemented"

defaultBackend :: (Monad f, HasCallStack) => FakeBucket f -> Backend (FakeBackend f)
defaultBackend bucket = Backend
  { _evalPackageOutput = \pkgId output -> do
      pkg <- preuse (field @"cache" . ix pkgId) >>= \case
        Nothing -> do
          pkg <- derivePackage bucket pkgId
          modify $ field @"cache" %~ (M.insert pkgId pkg)
          return pkg
        Just pkg -> return pkg
      case pkg ^? field @"derivation" . field @"outputs" . ix (outputIdText output) of
        Nothing  -> throwWithStack $ NoPackageOutput pkgId output
        Just out -> do
          setPackageDependency pkg output
          return $ out ^. field @"path" . re _Path
  , _storePath = \path -> do
      store <- asks _storeAdd
      res <- lift $ store path
      setPathDependency res
      return res
  , _storeBinary = \bin -> do
      store <- asks _storeAddBinary
      res <- lift $ store bin
      setPathDependency res
      return res
  , _failDerivation = throwWithStack . DerivationFailed
  }

derivePackage
  :: (Monad f, HasCallStack)
  => FakeBucket f
  -> PackageId
  -> FakeBackend f Package
derivePackage bucket pkgId = case bucket ^? field @"packages" . ix pkgId of
  Nothing  -> throwWithStack $ NoPackageFound pkgId
  Just drvM -> withPackage pkgId $ do
    checkNoRecursion
    ((builderPath, builderArgs, scriptPath), result) <- runDerivationM (defaultBackend bucket) $ do
      ((), script) <- listenScript drvM
      let ScriptResult interp scriptBin genArgs = runScript script
      builder <- packageFile interp
      scriptPath <- storeBinary scriptBin
      return (builder, genArgs scriptPath, scriptPath)
    cp <- stackHead
    let
      pkgDeps = cp ^. field @"packageDeps"
      srcDeps = cp ^. field @"srcDeps"
      defaultPlatform = platformText $ bucket ^. field @"platform"
      derivation = Derivation
        { outputs = result ^. field @"outputs"
          . to (M.fromList . fmap toOutput . M.toList)
        , inputDrvs = getInputDrvs pkgDeps
        , inputSrcs = S.insert (scriptPath ^. _Path) $ getInputSrcs srcDeps
        , platform  = maybe defaultPlatform platformText
          $ result ^. field @"platform"
        , builder = pathText builderPath
        , args = builderArgs
        , env = result ^. field @"env"
        }
      derivationBinary = encodeUtf8 $ toLazyText $ buildDerivation derivation
    drvPath <- nixStoreAddBinary derivationBinary
    let
      package = Package
        { packageId = pkgId
        , derivation = derivation
        , derivationPath = drvPath
        , packageDeps = pkgDeps
        , srcDeps = srcDeps
        }
    return package
  where
    toOutput (outId, output) = (outputIdText outId, drv)
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

getInputDrvs :: HasCallStack => PackageDeps -> Map F.FilePath (Set Text)
getInputDrvs = M.fromList . fmap go . M.toList
  where
    go :: (Package, Set OutputId) -> (F.FilePath, Set Text)
    go (pkg, outs) = (pkg ^. field @"derivationPath" . _Path, outPaths)
      where
        outPaths = S.fromList $ fmap outputIdText $ S.toList outs

getInputSrcs :: HasCallStack => SrcDeps  -> Set F.FilePath
getInputSrcs = S.fromList . fmap (view _Path) . S.toList
