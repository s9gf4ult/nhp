module NHP.Packages where

import           NHP.Imports
import           NHP.Monad
import           NHP.Packages.Hello
import           NHP.Types.Aux

allPackages :: (Monad f) => Either Text (PackageBucket f)
allPackages = addPackage "hello" hello (emptyBucket platformX86_64_linux)
