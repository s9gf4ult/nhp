module NHP.Types where

import           NHP.Imports
import qualified Nix.Derivation as Nix

newtype DerivationId = DerivationId
  { unDerivationId :: Int
  } deriving (Eq, Ord, Show, Enum)

data Derivation = Derivation
  { derivation :: Nix.Derivation
  } deriving (Eq, Ord, Show, Generic)

newtype Path = Path
  { pathText :: Text
  } deriving (Show, Eq, Ord, IsString)

-- | Unique name of output
newtype OutputId = OutputId
  { outputIdText :: Text
  } deriving (Show, Eq, Ord, IsString)

instance Default OutputId where
  def = "out"

-- | Output to create
data OutputType = SimpleOutput | FixedHashOutput Sha256
  deriving (Ord, Eq, Show)

data Sha256 = Sha256 ByteString
  deriving (Eq, Ord, Show)
