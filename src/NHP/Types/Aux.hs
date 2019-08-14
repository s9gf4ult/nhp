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
