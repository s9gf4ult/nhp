module NHP.Types where

import           Data.String
import           Data.Text
import           GHC.Generics

-- | Maybe just Text inside.
newtype PackageId = PackageId
  { unPackageId :: Text }
  deriving (Show, Eq, IsString)

-- | Baked package with resolved paths and dependencies built in. Can
-- not be changed, can be only resolved from package bucket. Have link
-- to the original package and bucket, so you always can reresolve it.
data Package = Package
  { packageId    :: PackageId
  , path         :: Path
  -- , pDerivation :: Derivation
  , dependencies :: [Package]
  } deriving (Generic)


newtype Path = Path
  { unPath :: Text
  } deriving (Show, Eq)

newtype Script = Script Text
  deriving (Eq, Ord, Semigroup, Monoid)

-- | Script level variable name
newtype Var a = Var Text
  deriving (Eq, Show)

data Url

data Sha256
