module BucketTest where

import           NHP.Imports
import           NHP.Monad
import           NHP.Types
import           Test.Tasty
import           Test.Tasty.HUnit

aPackage :: Bucket
aPackage = newPackage "a" $ do
  void $ defaultOutput

bPackage :: Bucket
bPackage = newPackage "b" $ do
  void $ evalPackageOutput "a" def

unit_AB :: HasCallStack => IO ()
unit_AB = do
  let
    Right (bucket, ()) = newBucketM platformX86_64_linux $ do
      aPackage
      bPackage
  r <- runResolveM fakeNix $ (,)
    <$> derivePackageId bucket "a"
    <*> derivePackageId bucket "b"
  case r of
    Right (a, b) ->
      assertBool "Not equal" $ a /= b
    Left e -> assertString $ show e
