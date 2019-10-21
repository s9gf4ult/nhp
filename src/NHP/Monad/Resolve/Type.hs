module NHP.Monad.Resolve.Type where

import           NHP.Imports
import           NHP.Monad.Types.Error
import           NHP.Types

-- | The resolve monad. It tracks package dependencies and calculates
-- the derivations
newtype ResolveM m a = ResolveM
  { unResolveM :: ReaderT (RMethods m) a
  } deriving
  ( Functor, Applicative, Monad, MonadReader (RMethods m), MonadTrans)

instance (Monad m) => RCall (RMethods m) m (m a) (ResolveM m a) where
  rCall apply = do
    r <- ask
    let v = apply r
    lift v
  {-# INLINE rCall #-}

data RMethods m = RMethods
  { currentPackage :: S m (DerivationState m)
  , withPackage    :: HasCallStack => PackageId -> ResolveM m a -> m a
  }

data DerivationState m
