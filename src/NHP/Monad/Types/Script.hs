module NHP.Monad.Types.Script where

data ScriptResult = ScriptResult
  { interpreter :: PackageFile
  -- ^ The interpreter to run the script
  , script      :: ByteString
  -- ^ The raw generated script
  , args        :: Path -> Vector Text
  -- ^ Generate the arguments for the interpreter Using the path of
  -- the script
  }

class RunScript a where
  runScript :: a ->
