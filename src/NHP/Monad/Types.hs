module NHP.Monad.Types
  ( module NHP.Monad.Types
  , module NHP.Monad.Types.Error
  ) where

import           Control.Monad.Trans.RWS.Strict
import           Filesystem.Path                as F
import           NHP.Imports
import           NHP.Monad.Types.Error
import           NHP.Script
import           NHP.Types

-- | The derivation monad. We dont derive the 'MonadReader' and
-- 'MonadState' for it to prevent the abuse of these interfaces, which
-- are too generic. The set of low-level methods must be consitent and
-- minimal. Also it must protect us from generating wrong derivations.
newtype DerivationM f a = DerivationM
  { unDerivation :: RWST (DrvMethods f) () DrvResult (ResolveM f) a
  } deriving
  ( Functor, Applicative, Monad )

-- | The resolve monad. It tracks package dependencies and calculates
-- the derivations
newtype ResolveM f a = ResolveM
  { unResolveM :: ExceptT (WithCallStack ResolveError) (RWST (NixBackend f) () ResolveState f) a
  } deriving
  ( Functor, Applicative, Monad, MonadError (WithCallStack ResolveError)
  , MonadState ResolveState, MonadReader (NixBackend f) )

-- | The bucket generation monad
newtype BucketM f a = BucketM
  { unBucketM :: ExceptT (WithCallStack BucketError) (RWS () () (BucketState f)) a
  } deriving
  ( Functor, Applicative, Monad
  , MonadState (BucketState f), MonadError (WithCallStack BucketError))

data BucketState f = BucketState
  { bucket :: Map PackageId (DerivationM f ())
  } deriving (Generic)

data DrvMethods f = DrvMethods
  { _evalPackageOutput :: HasCallStack => PackageId -> OutputId -> ResolveM f Path
  -- ^ Dependencies of the derivation are tracked by this monadic
  -- function. Any package becomes dependency of the current
  -- derivation if evaluated with this function.
  , _storePath         :: HasCallStack => F.FilePath -> ResolveM f Path
  -- ^ Store some file or directory in the store as fixed hash path
  -- and returns the path. The path will be added to the "inputSrcs"
  -- of the derivation
  , _storeBinary       :: HasCallStack => ByteString -> ResolveM f Path
  -- ^ Store some binary data in the store as file and give it a new
  -- path.
  , _failDerivation    :: forall a. HasCallStack => DerivationError -> ResolveM f a
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


type BucketMap f = Map PackageId (BucketElement f)

data BucketElement f
  = BucketDerivation (DerivationM f ())
  -- ^ Just2 a derivation
  | BucketClosure (DerivationM f ()) (BucketMap f)
  -- ^ Closure with main derivation and scope

data PackageBucket f = PackageBucket
  { packages :: BucketMap f
  , platform :: Platform
  -- ^ Default platform for all packages in the bucket. (If platform
  -- is not specified by the derivation)
  } deriving (Generic)

deriving instance (Monad f) => MonadReader (DrvMethods f) (DerivationM f)

instance MonadTrans DerivationM where
  lift ma = DerivationM $ lift $ lift ma

data ResolveState = ResolveState
  { cache :: Map PackagePoint Package
  -- ^ Already calculated packages
  , stack :: [CurrentPackage]
  } deriving (Generic)

type Scope = PackageId -> Maybe PackagePoint

data CurrentPackage = CurrentPackage
  { point       :: PackagePoint
  , scope       :: Scope
  , packageDeps :: PackageDeps
  , srcDeps     :: SrcDeps
  } deriving (Generic)

-- | The nix-store backend
data NixBackend f = NixBackend
  { _storeAdd       :: HasCallStack => F.FilePath -> f Path
  -- ^ Store derivation or other path in the Nix store
  , _storeAddBinary :: HasCallStack => ByteString -> f Path
  -- ^ Store binary data in the store and return the path
  }

instance MonadTrans ResolveM where
  lift ma = ResolveM $ lift $ lift ma
