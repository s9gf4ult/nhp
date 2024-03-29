module NHP.Types.Aux where

import           Data.List.NonEmpty        as NE
import qualified Data.Text                 as TS
import qualified Data.Text.Lazy            as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           NHP.Imports
import           Prelude                   hiding (FilePath)

newtype Path = Path
  { pathText :: Text
  } deriving (Show, Eq, Ord, IsString)

_Path :: Prism' Path FilePath
_Path = prism' b f
  where
    b = Path . T.fromStrict . either id id . toText
    f (Path p) =
      let res = fromText $ T.toStrict p
      in case valid res of
        False -> Nothing
        True  -> Just res

_PathText :: Prism' Text Path
_PathText = error "FIXME: _PathText not implemented"

(</>) :: Path -> Path -> Path
(</>) = error "FIXME: not implemented"

infixl </>

data Url


urlText :: Url -> Text
urlText = error "FIXME: urlText not implemented"

data Sha256 = Sha256 ByteString
  deriving (Eq, Ord, Show)

sha256Text :: Sha256 -> Text
sha256Text = error "FIXME: sha256Text not implemented"

newtype OutputId = OutputId
  { outputIdText :: Text
  } deriving (Show, Eq, Ord, IsString)

instance Default OutputId where
  def = "out"

data Output = SimpleOutput | FixedHashOutput Sha256
  deriving (Ord, Eq, Show)

newtype OutputPath = OutputPath
  { unOutputPath :: Text
  } deriving (Eq, Ord)

-- | TODO: make ADT for platform to generate only valid platform strings
newtype Platform = Platform
  { platformText :: TS.Text
  } deriving (Eq)

platformX86_64_linux :: Platform
platformX86_64_linux = Platform "x86_64-linux"

data License

-- | Maybe just Text inside.
newtype PackageId = PackageId
  { packageIdText :: Text
  } deriving (Show, Eq, Ord, IsString)

-- | Head is the package name, and the rest is the scope of the
-- package. So the last element is the first in terms of searching
-- package. The head is the most inner name of the package
newtype PackagePoint = PackagePoint (NonEmpty PackageId)
  deriving (Eq, Ord, Show)

packagePoint :: PackageId -> PackagePoint
packagePoint = PackagePoint . pure

ppAddInner :: PackageId -> [PackageId] -> PackagePoint
ppAddInner ppid ppids = PackagePoint $ ppid :| ppids

ppDirectPath :: PackagePoint -> NonEmpty PackageId
ppDirectPath (PackagePoint ne) = NE.reverse ne

ppFromDirectPath :: NonEmpty PackageId -> PackagePoint
ppFromDirectPath ne = PackagePoint $ NE.reverse ne

ppFromReversePath :: NonEmpty PackageId -> PackagePoint
ppFromReversePath = PackagePoint

-- | Some file in the package.
data PackageFile = PackageFile
  { package :: PackageId
  , output  :: OutputId
  , path    :: Path
  }
