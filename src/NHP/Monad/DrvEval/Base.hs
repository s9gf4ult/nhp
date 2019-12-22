module NHP.Monad.DrvEval.Base where

import           Control.Lens
import           Control.Monad.Except
import qualified Data.Map.Strict      as M
import           Filesystem.Path      as F
import           NHP.Imports
import           NHP.Types
import qualified Nix.Derivation       as Nix

-- | The most base monad for evaluation. Provides cache evaluation
-- cache and stuff to work with the nix daemon
newtype EvalBase m a = EvalBase
  { unEvalBase :: ReaderT (CacheMethods m, NixMethods m) m a
  } deriving (Functor, Applicative, Monad)

data CacheMethods m = CacheMethods
  { newDerivation :: HasCallStack => DerivationId -> Derivation -> m ()
  -- ^ The DerivationId must be gotten from the bucket element while evaluation.
  , getDerivation :: HasCallStack => DerivationId -> m (Maybe Derivation)
  -- ^ Get previously putted derivation from cache.
  }

-- | The nix-store backend
data NixMethods m = NixMethods
  { _storeAdd       :: HasCallStack => F.FilePath -> m Path
  -- ^ Store derivation or other path in the Nix store
  , _storeAddBinary :: HasCallStack => ByteString -> m Path
  -- ^ Store binary data in the store and return the path
  , _evalOutputPath
    :: HasCallStack
    => Nix.Derivation
    -- ^ Derivation with empty outputs
    -> OutputId
    -> OutputType
    -> m Nix.DerivationOutput
  }

data PureCacheState = PureCacheState
  { cache     :: Map DerivationId Derivation
  } deriving (Eq, Ord, Show, Generic)

type PureCacheMonad m = ExceptT CacheError (StateT PureCacheState m)

data CacheError = DrvAlreadyEvaled DerivationId

pureCachMethods :: (Monad m) => CacheMethods (PureCacheMonad m)
pureCachMethods = CacheMethods
  { newDerivation = \drvid drv -> do
      preuse (field @"cache" . ix drvid) >>= \case
        Nothing -> do
          field @"cache" %= M.insert drvid drv
        Just _ -> throwError $ DrvAlreadyEvaled drvid
  , getDerivation  = \drvid -> preuse $ field @"cache" . ix drvid
  }
