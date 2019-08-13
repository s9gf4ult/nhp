module NHP.MonadicExample where

import           NHP.Imports
import           NHP.Monad
import           NHP.Script
import           NHP.Types

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
