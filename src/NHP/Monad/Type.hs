module NHP.Monad.Type where

import NHP.Imports
import Control.Monad.Trans.RWS.Strict (RWST(..))
import Data.Text as T
import NHP.Imports
import NHP.Types
import NHP.Script
import Data.Map.Strict as M

data Backend f = Backend
  { _evalPackage    :: PackageId -> f Package
  -- ^ Dependencies of the derivation are tracked by this monadic
  -- function. Any package becomes dependency of the current
  -- derivation if evaluated with this function.
  , _evalDerivation :: forall a. DerivationM f a -> f (Package, a)
  -- ^ Same but for nested nameless derivations. Like derivation for
  -- downloads.
  , _failDerivation :: forall a. HasCallStack => Text -> f a
  -- ^ Fail the derivation with message.
  }

data DrvResult = DrvResult
  { script  :: Script
  , outputs :: Map OutputId Output
  } deriving (Generic)

newtype DerivationM f a = DerivationM
  { unDerivation :: RWST (Backend f) () DrvResult f a
  } deriving
  ( Functor, Applicative, Monad )

deriving instance (Monad f) => MonadReader (Backend f) (DerivationM f)

instance MonadTrans DerivationM where
  lift ma = DerivationM $ lift ma
