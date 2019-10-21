module NHP.Monad.Derivation.Type where

import           Filesystem.Path       as F
import           NHP.Imports
import           NHP.Monad.Types.Error
import           NHP.Script
import           NHP.Types

-- | The derivation monad. We dont derive the 'MonadReader' and
-- 'MonadState' for it to prevent the abuse of these interfaces, which
-- are too generic. The set of low-level methods must be consitent and
-- minimal. Also it must protect us from generating wrong derivations.
newtype DerivationM script (m :: * -> *) a = DerivationM (ReaderT (DMethods script m) m a)
  deriving (Functor, Applicative, Monad, MonadReader (DMethods script m))

instance MonadTrans (DerivationM script) where
  lift = DerivationM . lift

instance (Monad m) => RCall (DMethods script) m (m a) (DerivationM script m a) where
  rCall apply = do
    r <- ask
    let v = apply r
    lift v
  {-# INLINE rCall #-}

data DMethods script (m :: * -> *) = DMethods
  { evalPackage    :: HasCallStack => PackageId -> OutputId -> m Path
  -- ^ Dependencies of the derivation are tracked by this monadic
  -- function. Any package becomes dependency of the current
  -- derivation if evaluated with this function.
  , storePath      :: HasCallStack => F.FilePath -> m Path
  -- ^ Store some file or directory in the store as fixed hash path
  -- and returns the path. The path will be added to the "inputSrcs"
  -- of the derivation
  , storeBinary    :: HasCallStack => ByteString -> m Path
  -- ^ Store some binary data in the store as file and give it a new
  -- path.
  , failDerivation :: forall a. HasCallStack => DerivationError -> m a
  -- ^ Fail the derivation with message.
  , setOutput      :: HasCallStack => OutputId -> Output -> m ()
  , setPlatform    :: HasCallStack => Platform -> m ()
  , script         :: S m (Script script)
  -- ^ Script state
  , env            :: S m (Map Text Text)
  -- ^ Env state
  } deriving (Generic)
