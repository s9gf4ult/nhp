module NHP.Script where

import           Data.String
import           Data.Text   as T
import           NHP.Imports
import           NHP.Types

-- | Script in Lua (or Bash?)
data Script

instance Semigroup Script

instance Monoid Script

-- | Expression returning some @a@ in a script language
data Exp a

instance IsString (Exp Text) where
  fromString = strLit . T.pack

strLit :: Text -> Exp Text
strLit = error "FIXME: strLit not implemented"

pathLit :: Path -> Exp Path
pathLit = error "FIXME: pathLit not implemented"

toTextExp :: Exp a -> Exp Text
toTextExp = error "FIXME: toTextExp not implemented"
