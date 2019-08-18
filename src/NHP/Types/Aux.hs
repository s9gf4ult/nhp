module NHP.Types.Aux where

import           Filesystem.Path
import           NHP.Imports
import           Prelude         hiding (FilePath)

newtype Path = Path
  { unPath :: Text
  } deriving (Show, Eq, IsString)

_Path :: Prism' Path FilePath
_Path = error "FIXME: _Path not implemented"

(</>) :: Path -> Path -> Path
(</>) = error "FIXME: not implemented"

infixl </>

data Url


urlText :: Url -> Text
urlText = error "FIXME: urlText not implemented"

data Sha256 = Sha256 ByteString
  deriving (Eq, Ord)

sha256Text :: Sha256 -> Text
sha256Text = error "FIXME: sha256Text not implemented"

newtype OutputId = OutputId
  { unOutputId :: Text
  } deriving (Show, Eq, Ord, IsString)

instance Default OutputId where
  def = "out"

data Output = SimpleOutput | FixedHashOutput Sha256
  deriving (Ord, Eq)

newtype OutputPath = OutputPath
  { unOutputPath :: Text
  } deriving (Eq, Ord)

-- | TODO: make ADT for platform to generate only valid platform strings
newtype Platform = Platform
  { platformText :: Text
  } deriving (Eq)

platformX86_64_linux :: Platform
platformX86_64_linux = Platform "x86_64-linux"

data License
