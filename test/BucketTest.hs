module BucketTest where

import           NHP.Imports
import           NHP.Monad
import           NHP.Types
import           Test.Tasty
import           Test.Tasty.HUnit

base, aPackage, bPackage :: Monad f => Bucket f
base = do
  newPackage "bash" $ do
    void defaultOutput

aPackage = newPackage "a" $ do
  void defaultOutput

bPackage = newPackage "b" $ do
  void $ evalPackageOutput "a" def

unit_AB :: HasCallStack => IO ()
unit_AB = do
  let
    Right bucket = newBucketM platformX86_64_linux $ do
      base
      aPackage
      bPackage
  r <- runResolveM fakeNix $ (,)
    <$> derivePackageId bucket "a"
    <*> derivePackageId bucket "b"
  case r of
    Right (a, b) ->
      assertBool "Not equal" $ a /= b
    Left e -> assertBool (show e) False
