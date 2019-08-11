module NHP.Monad where

import Control.Monad.Trans.RWS.Strict (RWST(..))
import Data.Text as T
import NHP.Imports
import NHP.Types
import NHP.Script
import Data.Map.Strict as M

data DState = DState
  { nameCounter :: Integer
  } deriving Generic

data DWorld f = DWorld
  { buildPackage    :: PackageId -> f Package
  , buildDerivation :: forall a. DerivationM f a -> f (Package, a)
  }

data DResult = DResult
  { script  :: Script
  , outputs :: Map OutputId Output
  }

type Derivation f = DerivationM f ()

newtype DerivationM f a = DerivationM
  { unDerivation :: RWST (DWorld f) Script DState f a
  } deriving
  ( Functor, Applicative, Monad, MonadWriter Script
  , MonadState DState)

deriving instance (Monad f) => MonadReader (DWorld f) (DerivationM f)

instance MonadTrans DerivationM where
  lift ma = DerivationM $ lift ma

outFixedPath :: OutputId -> Sha256 -> DerivationM f (Exp Path)
outFixedPath = error "FIXME: not implemented"

outPath :: (Monad f) => OutputId -> DerivationM f (Exp Path)
outPath = error "FIXME: outPath not implemented"

evalDerivation :: (Monad f) => DerivationM f a -> DerivationM f (Package, a)
evalDerivation drv = do
  bd <- asks buildDerivation
  lift $ bd drv

-- | Set dependency on given package
package :: (Monad f) => PackageId -> DerivationM f Package
package pkgid = do
  g <- asks buildPackage
  lift $ g pkgid

failDerivation :: HasCallStack => Text -> DerivationM f a
failDerivation = error "FIXME: not implemented"

getPackageOutput :: (Monad f) => Package -> OutputId -> DerivationM f Path
getPackageOutput pkg out = case pkg ^? field @"outputs" . ix out of
  Nothing -> failDerivation
    $ "Not found out " <> unOutputId out <> " in package " <> unPackageId (packageId pkg)
  Just p  -> return p

-- | Gets binary from specified package and sets dependency on it.
packageBin :: (Monad f) => PackageId -> OutputId -> Path -> DerivationM f Path
packageBin pkgid out binName = do
  pkg <- package pkgid
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
