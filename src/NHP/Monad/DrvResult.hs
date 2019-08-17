module NHP.Monad.DrvResult where

import           NHP.Imports
import           NHP.Monad.Type
import           NHP.Script
import           NHP.Types


writeScript :: Script -> DerivationM f ()
writeScript = error "FIXME: writeScript not implemented"

setOutput :: OutputId -> Output -> DerivationM f ()
setOutput = error "FIXME: setOutput not implemented"

setFixedHashOutput :: OutputId -> Output -> Sha256 -> DerivationM f ()
setFixedHashOutput = error "FIXME: setFixedHashOutput not implemented"
