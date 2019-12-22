module NHP.Monad.BucketGen.Base where

import           NHP.Imports
import           NHP.Types

-- | The most base monad for bucket generation. Provides derivation id generation.
newtype BucketBase m a = BucketBase
  { unBucketBase :: ReaderT (BaseMethods m) m a
  } deriving (Functor, Applicative, Monad)

data BaseMethods m = BaseMethods
  { _newDrvId :: HasCallStack => m DerivationId
  }

type PureBaseMonad m = StateT DerivationId m

pureBaseMethods :: (Monad m) => BaseMethods (PureBaseMonad m)
pureBaseMethods = BaseMethods
  { _newDrvId = do
      did <- get
      put $ succ did
      return did
  }

-- newDrvId :: BucketBase m DerivationId
-- newDrvId = error "FIXME: newDrvId not implemented"
