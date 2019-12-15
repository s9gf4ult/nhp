module NHP.Types where

import           NHP.Imports
import qualified Nix.Derivation as Nix

newtype DerivationId = DerivationId
  { unDerivationId :: Int
  } deriving (Eq, Ord, Show, Enum)

data Derivation = Derivation
  { derivation :: Nix.Derivation
  } deriving (Eq, Ord, Show, Generic)
