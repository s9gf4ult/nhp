module NHP.Packages where

import           NHP.Bucket
import           NHP.Imports
import           NHP.Packages.Hello
import           NHP.Types.Aux

allPackages :: (Monad f) => Either Text (PackageBucket f)
allPackages = addPackage "hello" hello (emptyBucket platformX86_64_linux)
