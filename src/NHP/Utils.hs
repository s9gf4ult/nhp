module NHP.Utils where

import           NHP.Imports
import           NHP.Monad
import           NHP.Script
import           NHP.Types

outFixedPath :: OutputId -> Sha256 -> DerivationM f (Exp Path)
outFixedPath = error "FIXME: not implemented"

outPath :: (Monad f) => OutputId -> DerivationM f (Exp Path)
outPath = error "FIXME: outPath not implemented"

-- | Gets binary from specified package and sets dependency on it.
packageBin :: (Monad f) => PackageId -> OutputId -> Path -> DerivationM f Path
packageBin pkgid out binName = do
  pkg <- evalPackage pkgid
  path <- getPackageOutput pkg out
  return $ path </> "bin" </> binName

-- | Generates script calling binary with given arguments
callBin :: (Monad f) => Path -> [Exp Text] -> DerivationM f ()
callBin = error "FIXME: not implemented"

-- | Call @curl@ binary from @curl@ package for example.
simpleCallBin :: (Monad f) => PackageId -> [Exp Text] -> DerivationM f ()
simpleCallBin pkgId args = do
  p <- packageBin pkgId def (Path $ unPackageId pkgId)
  callBin p args

-- | Wraps script in @pushd@ and @popd@ so internal computation works
-- inside given directory
within :: Exp Path -> DerivationM f a -> DerivationM f a
within = error "FIXME: within not implemented"

-- | Fetch fixed hash derivation. The path is known at eval state.
fetchUrl :: Monad f => Url -> Sha256 -> DerivationM f Path
fetchUrl url sha = do
  (pkg, outId) <- evalDerivation $ deriveFetchUrl url sha
  getPackageOutput pkg outId

deriveFetchUrl :: (Monad f) => Url -> Sha256 -> DerivationM f OutputId
deriveFetchUrl url sha = do
  let out = def
  outPath <- outFixedPath out sha
  simpleCallBin "curl" ["-o", toTextExp outPath, strLit $ urlText url]
  return def

-- | Unpacks archive with @tar@ and puts internals in one directory.
unTar
  :: (Monad f)
  => Exp Path
  -- ^ Archive path
  -> Exp Path
  -- ^ Output directory path
  -> DerivationM f ()
unTar = error "FIXME: unZip not implemented"

data Configure

defaultConfigure
  :: Exp Path
  -- ^ Prefix for the output
  -> Configure
defaultConfigure = error "FIXME: defaultConfigure not implemented"

-- | Runs @configure@ script with arguments
configure :: Configure ->  DerivationM f ()
configure = error "FIXME: configure not implemented"

make :: [Exp Text] -> DerivationM f ()
make = error "FIXME: make not implemented"

mkTmpDir :: DerivationM f (Exp Path)
mkTmpDir = error "FIXME: mkTmpDir not implemented"
