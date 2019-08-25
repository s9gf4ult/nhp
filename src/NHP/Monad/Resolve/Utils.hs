module NHP.Monad.Resolve.Utils where

import           Data.Map.Strict         as M
import           Data.Set                as S
import           Data.Text.Lazy.Builder  as TB
import           Data.Text.Lazy.Encoding as TE
import           Filesystem.Path         as F
import           NHP.Error
import           NHP.Imports
import           NHP.Monad.Derivation
import           NHP.Monad.Types
import           NHP.Script
import           NHP.State
import           NHP.Types
import           Prelude                 as P

emptyStackFailure :: (Monad f, HasCallStack) => ResolveM f a
emptyStackFailure = throwWithStack
  $ EvalAssertionFailed "Current evaluating package is empty"

currentPackage :: PackageId -> CurrentPackage
currentPackage pkgId = CurrentPackage pkgId M.empty S.empty

stackHead :: (Monad f, HasCallStack) => ResolveM f CurrentPackage
stackHead = preuse (field @"stack" . _head) >>= \case
  Nothing -> emptyStackFailure
  Just cp -> return cp

modifyStackHead
  :: (Monad f, HasCallStack, m ~ ResolveM f)
  => (HasCallStack => CurrentPackage -> m (CurrentPackage, a))
  -> m a
modifyStackHead f = do
  use (field @"stack") >>= \case
    [] -> emptyStackFailure
    (h:t) -> do
      (newH, a) <- f h
      modify $ field @"stack" .~ (newH:t)
      return a

setPackageDependency :: (Monad f, HasCallStack) => Package -> OutputId -> ResolveM f ()
setPackageDependency pkg outId = modifyStackHead $ \cp -> do
  let
    addDep = M.insertWith S.union pkg (S.singleton outId)
    newCp = cp & field @"packageDeps" %~ addDep
  return (newCp, ())

setPathDependency :: (Monad f, HasCallStack) => Path -> ResolveM f ()
setPathDependency path = modifyStackHead $ \cp -> do
  let
    addDep = S.insert path
    newCp = cp & field @"srcDeps" %~ addDep
  return (newCp, ())

nixStoreAdd :: (Monad f, HasCallStack) => F.FilePath -> ResolveM f Path
nixStoreAdd fp = do
  store <- asks _storeAdd
  lift $ store fp

nixStoreAddBinary :: (Monad f, HasCallStack) => ByteString -> ResolveM f Path
nixStoreAddBinary bs = do
  store <- asks _storeAddBinary
  lift $ store bs

withPackage :: (Monad f, HasCallStack) => PackageId -> ResolveM f a -> ResolveM f a
withPackage pkgId ma = do
  (a, newS) <- listenState $ do
    modify $ field @"stack" %~ ((currentPackage pkgId) :)
    ma
  modify $ field @"cache" .~ (newS ^. field @"cache")
  return a

-- | Check that there is no recursion in the package stack.
checkNoRecursion :: (Monad f, HasCallStack) => ResolveM f ()
checkNoRecursion = do
  use (field @"stack") >>= \case
    [] -> emptyStackFailure
    s@(h: t)
      | elem h t  -> throwWithStack $ CircularDependencies
        $ s ^.. folded . field @"packageId"
      | otherwise -> return ()

drvMethods
  :: (Monad f, HasCallStack)
  => PackageBucket f
  -> DrvMethods f
drvMethods bucket = DrvMethods
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
  => PackageBucket f
  -> PackageId
  -> ResolveM f Package
derivePackage bucket pkgId = case bucket ^? field @"packages" . ix pkgId of
  Nothing  -> throwWithStack $ NoPackageFound pkgId
  Just drvM -> withPackage pkgId $ do
    checkNoRecursion
    ((builderPath, builderArgs, scriptPath), result) <- runDerivationM (drvMethods bucket) $ do
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
