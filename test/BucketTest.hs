module BucketTest where

import           NHP.Imports
import           NHP.Monad
import           NHP.Types
import           Test.Tasty
import           Test.Tasty.HUnit

unit_Can_Resolve :: HasCallStack => IO ()
unit_Can_Resolve = do
  let
    Right bucket = newBucketM platformX86_64_linux $ do
      newPackage "a" $ do
        noopScript
        void defaultOutput
      newPackage "b" $ do
        noopScript
        void $ evalPackageOutput "a" def
  r <- runResolveM fakeNix $ (,)
    <$> derivePackageId bucket "a"
    <*> derivePackageId bucket "b"
  case r of
    Right (a, b) -> do
      assertBool "Not equal" $ a /= b
      assertBool (show b) $ b ^.. field @"packageDeps" .

    Left e -> assertBool (show e) False

unit_No_Output_Fails :: HasCallStack => IO ()
unit_No_Output_Fails = do
  let
    Right bucket = newBucketM platformX86_64_linux $ do
      newPackage "a" $ do
        noopScript
        -- No output, 'b' must fail
      newPackage "b" $ do
        noopScript
        void $ evalPackageOutput "a" def
  r <- runResolveM fakeNix $
    derivePackageId bucket "b"
  case r of
    Right a -> assertBool (show a) False
    Left (WithCallStack (NoPackageOutput "a" o) _)
      | o == def -> return ()
    Left e -> assertBool (show e) False
