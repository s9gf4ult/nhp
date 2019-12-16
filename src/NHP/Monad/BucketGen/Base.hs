module NHP.Monad.BucketGen.Base where

import           NHP.Imports
import           NHP.Types

-- | The most base monad for bucket generation. Provides derivation id generation.
newtype BucketBase m a = BucketBase
  { unBucketBase :: ReaderT (BaseMethods m) m a
  } deriving (Functor, Applicative, Monad)

data BaseMethods m = BaseMethods
  { newDrvId :: m DerivationId
  } deriving (Generic)

type PureBaseMonad m = StateT DerivationId m

pureBaseMethods :: (Monad m) => BaseMethods (PureBaseMonad m)
pureBaseMethods = BaseMethods
  { newDrvId = do
      did <- get
      put $ succ did
      return did
  }
