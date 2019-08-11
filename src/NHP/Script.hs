module NHP.Script where

import           NHP.Imports

-- | Script in Lua
data Script

-- | Expression returning some @a@ in a script language
data Exp a

strLit :: Text -> Exp Text
strLit = error "FIXME: strLit not implemented"

callBin :: Path -> [Exp Text] -> Script
callBin = error "FIXME: not implemented"
