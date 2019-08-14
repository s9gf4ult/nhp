module NHP.Packages where

import           NHP.Bucket
import           NHP.Imports
import           NHP.Packages.Hello

allPackages :: (Monad f) => Either Text (PackageBucket f)
allPackages = addPackage "hello" hello emptyBucket
