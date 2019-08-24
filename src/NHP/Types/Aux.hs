module NHP.Types.Aux where

import           Filesystem.Path
import           NHP.Imports
import           Prelude         hiding (FilePath)

newtype Path = Path
  { pathText :: Text
  } deriving (Show, Eq, Ord, IsString)

_Path :: Prism' Path FilePath
_Path = error "FIXME: _Path not implemented"

_PathText :: Prism' Text Path
_PathText = error "FIXME: _PathText not implemented"

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
  { outputIdText :: Text
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

-- | Maybe just Text inside.
newtype PackageId = PackageId
  { packageIdText :: Text
  } deriving (Show, Eq, Ord, IsString)

-- | Some file in the package.
data PackageFile = PackageFile
  { package :: PackageId
  , output  :: OutputId
  , path    :: Path
  }

data ScriptResult = ScriptResult
  { interpreter :: PackageFile
  -- ^ The interpreter to run the script
  , script      :: ByteString
  -- ^ The raw generated script
  , args        :: Path -> Vector Text
  -- ^ Generate the arguments for the interpreter Using the path of
  -- the script
  }
