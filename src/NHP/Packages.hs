module NHP.Packages where

import           NHP.Imports
import           NHP.Monad
import           NHP.Packages.Hello
import           NHP.Types.Aux

allPackages :: (Monad f, HasCallStack) => BucketM f ()
allPackages = do
  hello
