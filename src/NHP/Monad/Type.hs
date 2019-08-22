module NHP.Monad.Type where

import           Control.Monad.Trans.RWS.Strict (RWST (..))
import           Data.Map.Strict                as M
import           Data.Text                      as T
import           NHP.Imports
import           NHP.Imports
import           NHP.Script
import           NHP.Types

-- | Multiple output declarations.
data OutputExistsError = OutputExistsError
  { outputId  :: OutputId
  , oldOutput :: Output
  , newOutput :: Output
  } deriving (Eq, Ord, Generic)

data DerivationFail
  = PlatformNotSupported Text
  -- ^ This derivation is will not work on this platform
  | PackageBroken Text
  -- ^ The program will not work correctly or build script will fail
  -- to build the derivation.
  | OutputAlreadyExists OutputExistsError
  | SomeFail Text
  -- ^ Eval time failure
  deriving (Ord, Eq, Generic)

data Backend f = Backend
  { _evalPackageOutput :: HasCallStack => PackageId -> OutputId -> f Path
  -- ^ Dependencies of the derivation are tracked by this monadic
  -- function. Any package becomes dependency of the current
  -- derivation if evaluated with this function.
  , _storePath         :: HasCallStack => FilePath -> f Path
  -- ^ Store some file or directory in the store as fixed hash path
  -- and returns the path. The path will be added to the "inputSrcs"
  -- of the derivation
  , _storeBinary       :: HasCallStack => ByteString -> f Path
  -- ^ Store some binary data in the store as file and give it a new
  -- path.
  , _failDerivation    :: forall a. HasCallStack => DerivationFail -> f a
  -- ^ Fail the derivation with message.
  }

data DrvResult = DrvResult
  { script   :: Script
  -- ^ Building script
  , outputs  :: Map OutputId Output
  -- ^ Outputs of the derivation.
  , license  :: Maybe License
  , platform :: Maybe Platform
  , env      :: Map Text Text
  } deriving (Generic)

emptyResult :: DrvResult
emptyResult = DrvResult
  { script   =  mempty
  , outputs  = mempty
  , license  = Nothing
  , platform = Nothing
  , env      = mempty
  }

-- | The derivation monad. We dont derive the 'MonadReader' and
-- 'MonadState' for it to prevent the abuse of these interfaces, which
-- are too generic. The set of low-level methods must be consitent and
-- minimal. Also it must protect us from generating wrong derivations.
newtype DerivationM f a = DerivationM
  { unDerivation :: RWST (Backend f) () DrvResult f a
  } deriving
  ( Functor, Applicative, Monad )

deriving instance (Monad f) => MonadReader (Backend f) (DerivationM f)

instance MonadTrans DerivationM where
  lift ma = DerivationM $ lift ma

runDerivationM :: Monad f => Backend f -> DerivationM f a -> f (a, DrvResult)
runDerivationM backend drv = drop <$> runRWST (unDerivation drv) backend emptyResult
  where
    drop (a, b, _) = (a, b)
