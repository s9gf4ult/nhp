module NHP.Monad.Resolve.Utils where

import           Data.List.NonEmpty      as NE
import           Data.Map.Strict         as M
import           Data.Set                as S
import           Data.Text               as TS
import           Data.Text.Lazy          as T
import           Data.Text.Lazy.Builder  as TB
import           Data.Text.Lazy.Encoding as TE
import           Filesystem.Path         as F
import           NHP.Error
import           NHP.Imports
import           NHP.Monad.Derivation
import           NHP.Monad.Types
import           NHP.Script
import           NHP.Types
import           Prelude                 as P

emptyStackFailure :: (Monad f, HasCallStack) => ResolveM f a
emptyStackFailure = throwWithStack
  $ EvalAssertionFailed "Current evaluating package is empty"

currentPackage :: PackagePoint -> Scope -> CurrentPackage
currentPackage ppoint scope = CurrentPackage
  { point       = ppoint
  , scope       = scope
  , packageDeps = M.empty
  , srcDeps     = S.empty
  }

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
    []    -> emptyStackFailure
    (h:t) -> do
      (newH, a) <- f h
      modify $ field @"stack" .~ (newH:t)
      return a

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

nixStoreEvalOutputPath
  :: (Monad f, HasCallStack)
  => Derivation
  -> OutputId
  -> Output
  -> ResolveM f DerivationOutput
nixStoreEvalOutputPath drv oid out = do
  eval <- asks _evalOutputPath
  lift $ eval drv oid out

nixStoreAddBinary :: (Monad f, HasCallStack) => ByteString -> ResolveM f Path
nixStoreAddBinary bs = do
  store <- asks _storeAddBinary
  lift $ store bs

withPackage
  :: (Monad f, HasCallStack)
  => BucketMap f
  -> PackagePoint
  -> (SomeDerivation f () -> ResolveM f a)
  -> ResolveM f a
withPackage bucket ppoint ma = do
  (a, newS) <- listenState $ do
    (scope, drv) <- lookupPackagePoint bucket ppoint
    modify $ field @"stack" %~ ((currentPackage ppoint scope) :)
    ma drv
  modify $ field @"cache" .~ (newS ^. field @"cache")
  -- Prevent the cac he from loosing
  return a

-- | Check that there is no recursion in the package stack.
checkNoRecursion :: (Monad f, HasCallStack) => ResolveM f ()
checkNoRecursion = do
  a <- get
  case a ^.. field @"stack" . traversed . field @"point" of
    [] -> emptyStackFailure
    s@(h: t)
      | elem h t  -> throwWithStack $ CircularDependencies s
      | otherwise -> return ()

-- | Goes deep into the bucket map and collects the scope of all
-- closures met in the pass. Returns the scope of derivation and
-- derivation itself.
lookupPackagePoint
  :: forall f script
  . (Monad f, HasCallStack)
  => BucketMap f
  -> PackagePoint
  -- ^ Point to find the derivation at
  -> ResolveM f (Scope, SomeDerivation f ())
lookupPackagePoint bucket ppoint =
  go [] (const Nothing) bucket (ppDirectPath ppoint)
  where
    go
      :: [PackageId]
      -- ^ Path we already passed. The head is the most deep name
      -> Scope
      -- ^ The scope function
      -> BucketMap f
      -- ^ Current closure map
      -> NonEmpty PackageId
      -- ^ Reversed pp. The head is the most top level name
      -> ResolveM f (Scope, SomeDerivation f ())
    go breadCrumbs topScope topMap (name :| rest) = do
      let
        curPoint = ppAddInner name breadCrumbs
        retScope = mkScope breadCrumbs topScope topMap
      case topMap ^? ix name of
        Nothing  -> throwWithStack $ NoPackageFound curPoint
        Just elt -> case rest of
          [] -> case elt of
            BucketDerivation drv         -> return (retScope, drv)
            BucketClosure drv closureMap -> return
              ( mkScope (name : breadCrumbs) retScope closureMap
              , drv )
          (newName:newRest) -> case elt of
            BucketDerivation _       ->
              throwWithStack $ ClosureExpected curPoint
            BucketClosure _drv closureMap -> do
              go (name : breadCrumbs) retScope closureMap (newName :| newRest)
    mkScope brd oldScope closureMap pkgId = case closureMap ^? ix pkgId of
      Just _elt -> Just $ ppAddInner pkgId brd
      Nothing   -> oldScope pkgId

drvMethods
  :: (Monad f, HasCallStack)
  => PackageBucket f
  -> DrvMethods f
drvMethods bucket = DrvMethods
  { _evalPackageOutput = \pkgId output -> modifyStackHead $ \cp -> do
      case (cp ^. field @"scope") pkgId of
        Nothing -> throwWithStack $ PackageNotInScope pkgId
        Just ppoint -> do
          pkg <- preuse (field @"cache" . ix ppoint) >>= \case
            Nothing -> do
              pkg <- derivePackage bucket ppoint
              modify $ field @"cache" %~ (M.insert ppoint pkg)
              return pkg
            Just pkg -> return pkg
          case pkg ^? field @"derivation" . field @"outputs" . ix (T.toStrict $ outputIdText output) of
            Nothing  -> throwWithStack $ NoPackageOutput pkgId output
            Just out -> do
              let newCP = cp & field @"packageDeps"
                    %~ M.insertWith S.union pkg (S.singleton output)
                    -- insert package dependency
              return (newCP, out ^. field @"path" . re _Path)
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

derivePackageId
  :: (Monad f, HasCallStack)
  => PackageBucket f
  -> PackageId
  -> ResolveM f Package
derivePackageId bucket pkgId = derivePackage bucket (packagePoint pkgId)

derivePackage
  :: (Monad f, HasCallStack)
  => PackageBucket f
  -> PackagePoint
  -> ResolveM f Package
derivePackage bucket ppoint = withPackage (bucket ^. field @"packages") ppoint $ \(SomeDerivation drvM) -> do
  checkNoRecursion
  ((builderText, builderArgs), result) <- runDerivationM (drvMethods bucket) $ do
    ((), script) <- listenScript drvM
    let ScriptResult interp scriptBin genArgs = runScript script
    builder <- case interp of
      BuiltinBuilder t    -> return t
      ExecutableBuilder p -> pathText <$> packageFile p
    scriptPath <- traverse storeBinary scriptBin
    return (builder, genArgs scriptPath)
  cp <- stackHead
  let
    pkgDeps = cp ^. field @"packageDeps"
    srcDeps = cp ^. field @"srcDeps"
    defaultPlatform = platformText $ bucket ^. field @"platform"
    preDerivation = Derivation
      { outputs   = M.empty
      , inputDrvs = getInputDrvs pkgDeps
      , inputSrcs = getInputSrcs srcDeps
      , platform  = maybe defaultPlatform platformText
        $ result ^. field @"platform"
      , builder   = T.toStrict builderText
      , args      = builderArgs
      , env       = result ^. field @"env"
      }
  outputs <- for (result ^. field @"outputs". to M.toList) $ \(outId, out) -> do
    path <- nixStoreEvalOutputPath preDerivation outId out
    return (T.toStrict $ outputIdText outId, path)
  let
    derivation = preDerivation & field @"outputs" .~ M.fromList outputs
    derivationBinary = encodeUtf8 $ toLazyText $ buildDerivation derivation
  drvPath <- nixStoreAddBinary derivationBinary
  let
    package = Package
      { point          = ppoint
      , derivation     = derivation
      , derivationPath = drvPath
      , packageDeps    = pkgDeps
      , srcDeps        = srcDeps }
  return package

getInputDrvs :: HasCallStack => PackageDeps -> Map F.FilePath (Set TS.Text)
getInputDrvs = M.fromList . fmap go . M.toList
  where
    go :: (Package, Set OutputId) -> (F.FilePath, Set TS.Text)
    go (pkg, outs) =
      ( pkg ^. field @"derivationPath" . _Path
      , outPaths)
      where
        outPaths = S.fromList $ fmap (T.toStrict . outputIdText) $ S.toList outs

getInputSrcs :: HasCallStack => SrcDeps  -> Set F.FilePath
getInputSrcs = S.fromList . fmap (view _Path) . S.toList
