module NHP.Types where

import           Data.Default
import           Data.String
import           Data.Text
import           GHC.Generics
import           NHP.Imports

-- | Maybe just Text inside.
newtype PackageId = PackageId
  { unPackageId :: Text }
  deriving (Show, Eq, IsString)

-- | Baked package with resolved paths and dependencies built in. Can
-- not be changed, can be only resolved from package bucket. Have link
-- to the original package and bucket, so you always can reresolve it.
data Package = Package
  { packageId    :: PackageId
  , name         :: DerivationName
  , path         :: Path
  , outputs      :: Map OutputId Path
  , dependencies :: [Package]
  } deriving (Generic)

newtype DerivationName = DerivationName
  { unDerivationName :: Text
  } deriving (Eq, Ord, IsString)

newtype Path = Path
  { unPath :: Text
  } deriving (Show, Eq, IsString)

(</>) :: Path -> Path -> Path
(</>) = error "FIXME: not implemented"

infixl </>

data Url


urlText :: Url -> Text
urlText = error "FIXME: urlText not implemented"

data Sha256

newtype OutputId = OutputId
  { unOutputId :: Text
  } deriving (Show, Eq, Ord, IsString)

instance Default OutputId where
  def = "out"

data Output = SimpleOutput | FixedHashOutput Sha256

newtype OutputPath = OutputPath
  { unOutputPath :: Text
  } deriving (Eq, Ord)
