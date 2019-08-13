module NHP.Types where

import           Data.Default
import           Data.String
import           Data.Text
import           GHC.Generics
import           NHP.Imports


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
