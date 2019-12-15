module NHP.Monad.DrvEval.Cache where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map.Strict      as M
import           NHP.Imports
import           NHP.Types

-- | The most base monad for evaluation. Provides cache evaluation
-- cache and stuff to work with the nix daemon
newtype DrvCache m a = DrvCache
  { unDrvCache :: ReaderT (CacheMethods m) m a
  } deriving (Functor, Applicative, Monad)

data CacheMethods m = CacheMethods
  { newDerivation :: DerivationId -> Derivation -> m ()
  -- ^ The DerivationId must be gotten from the bucket element while evaluation.
  , getDerivation :: DerivationId -> m (Maybe Derivation)
  -- ^ Get previously putted derivation from cache.
  } deriving (Generic)

data PureCache = PureCache
  { cache     :: Map DerivationId Derivation
  } deriving (Eq, Ord, Show, Generic)

type PureCacheMonad m = ExceptT CacheError (StateT PureCache m)

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
