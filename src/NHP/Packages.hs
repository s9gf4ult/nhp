module NHP.Packages where

import           NHP.Imports
import           NHP.Monad
import           NHP.Packages.Hello

allPackages :: (Monad f, HasCallStack) => BucketM f ()
allPackages = do
  hello
