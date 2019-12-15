module NHP.Monad.BucketGen.Base where


-- | The most base monad for bucket generation. Provides derivation id generation.
newtype BucketBase = BucketBase
  { unBucketBase :: ReaderT
  } deriving (Eq)
