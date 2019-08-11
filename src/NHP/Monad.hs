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
  { writeScript :: Script
  , writeOutputs :: Map OutputId Output
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

outFixedPath :: OutputId -> DerivationM f (Var Path)
outFixedPath = error "FIXME: not implemented"

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
packageBin :: (Monad f) => PackageId -> Path -> DerivationM f Path
packageBin pkgid binName = do
  pkg <- package pkgid
  path <- getPackageOutput pkg def
  return $ path </> "bin" </> binName

newName :: (Monad f) => Text -> DerivationM f (Var a)
newName n = do
  st <- get
  let c = nameCounter st
  put $ st { nameCounter = succ c }
  return $ Var $ n <> (T.pack $ show c)
