module NHP.Utils where

import           NHP.Imports
import           NHP.Monad
import           NHP.Script
import           NHP.Types

-- | Generates script calling binary with given arguments
callBin :: (Monad f) => Path -> [Exp Text] -> DerivationM script f ()
callBin = error "FIXME: not implemented"

-- | Call @curl@ binary from @curl@ package for example.
simpleCallBin :: (Monad f) => PackageId -> [Exp Text] -> DerivationM script f ()
simpleCallBin pkgId args = do
  let binName = Path $ packageIdText pkgId
  -- | Binary named just like package
  p <- packageFile $ PackageFile pkgId def $ "/bin" </> binName
  callBin p args

-- | Wraps script in @pushd@ and @popd@ so internal computation works
-- inside given directory
within :: Exp Path -> DerivationM script f a -> DerivationM script f a
within = error "FIXME: within not implemented"

-- -- | Fetch fixed hash derivation. The path is known at eval state.
-- fetchUrl :: (Monad f, HasCallStack) => Url -> Sha256 -> DerivationM f Path
-- fetchUrl url sha = do
--   outs <- evalDerivation (deriveFetchUrl url sha) [def]
--   maybe (failDerivation $ NoOutputFound def) return
--     $ outs ^? ix def

deriveFetchUrl :: (Monad f) => Url -> Sha256 -> DerivationM Bash f ()
deriveFetchUrl url sha = do
  let out = def
  outPath <- setOutput out $ FixedHashOutput sha
  simpleCallBin "curl" ["-o", toTextExp outPath, strLit $ urlText url]

-- | Unpacks archive with @tar@ and puts internals in one directory.
unTar
  :: (Monad f)
  => Exp Path
  -- ^ Archive path
  -> Exp Path
  -- ^ Output directory path
  -> DerivationM Bash f ()
unTar = error "FIXME: unZip not implemented"

data Configure

defaultConfigure
  :: Exp Path
  -- ^ Prefix for the output
  -> Configure
defaultConfigure = error "FIXME: defaultConfigure not implemented"

-- | Runs @configure@ script with arguments
configure :: Configure ->  DerivationM script f ()
configure = error "FIXME: configure not implemented"

make :: [Exp Text] -> DerivationM script f ()
make = error "FIXME: make not implemented"

mkTmpDir :: DerivationM script f (Exp Path)
mkTmpDir = error "FIXME: mkTmpDir not implemented"
