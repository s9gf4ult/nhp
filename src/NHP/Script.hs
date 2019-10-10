module NHP.Script where

import           Data.String
import qualified Data.Text               as TS
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding as T
import           NHP.Imports
import           NHP.Types

data Bash = Bash Text

-- | Script in Lua (or Bash?)
data Script script = Script
  { scriptRunner :: script -> ScriptResult
  -- ^ Final script runner. Here to be able to replace the function in
  -- the end.
  , script       :: script
  -- ^ The script
  } deriving (Generic)

runScript :: Script script -> ScriptResult
runScript (Script runner script) = runner script

instance Default (Script ()) where
  def = Script
    { scriptRunner = \() -> ScriptResult
      { builder = BuiltinBuilder "noop"
      , script  = Nothing
      , args    = const mempty
      }
    , script = ()
    }

instance Default (Script Bash) where
  def = Script
    { scriptRunner = \(Bash script) -> ScriptResult
      { builder = ExecutableBuilder bashExecutable
      , script  = Just $ T.encodeUtf8 script
      , args    = error "NOT Implemented"
      }
    , script = Bash mempty
    }

type DefScript script = Default (Script script)

data Builder
  = BuiltinBuilder Text
  | ExecutableBuilder PackageFile

data ScriptResult = ScriptResult
  { builder :: Builder
  -- ^ The interpreter to run the script
  , script  :: Maybe ByteString
  -- ^ The raw generated script
  , args    :: Maybe Path -> Vector TS.Text
  -- ^ Generate the arguments for the interpreter Using the path of
  -- the script
  }

bashExecutable :: PackageFile
bashExecutable = PackageFile
  { package = "bash"
  , output = def
  , path = "/bin/bash"
  }

-- instance Semigroup Script

-- instance Monoid Script

-- | Expression returning some @a@ in a script language
data Exp a

instance IsString (Exp Text) where
  fromString = strLit . T.pack

strLit :: Text -> Exp Text
strLit = error "FIXME: strLit not implemented"

pathLit :: Path -> Exp Path
pathLit = error "FIXME: pathLit not implemented"

outputVar :: OutputId -> Exp Path
outputVar = error "FIXME: outputVar not implemented"

toTextExp :: Exp a -> Exp Text
toTextExp = error "FIXME: toTextExp not implemented"

bashInterpreter :: PackageFile
bashInterpreter = PackageFile "bash" "out" "/bin/bash"
