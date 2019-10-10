module BucketTest where

import           NHP.Imports
import           NHP.Monad
import           NHP.Types
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Show.Pretty

-- type UnitTest = HasCallStack => IO ()

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
    Right (a, b :: Package) -> do
      assertBool "Not equal" $ a /= b
      --  FIXME: Weird. Looks like the 'a' evaluated multiple times
      -- assertBool (unlines [ppShow a, ppShow b]) $
      --   has (field @"packageDeps" . ix a . ix def) b
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

unit_Recursion_Fails :: HasCallStack => IO ()
unit_Recursion_Fails = do
  let
    Right bucket = newBucketM platformX86_64_linux $ do
      newPackage "a" $ do
        noopScript
        void defaultOutput
        void $ evalPackageOutput "b" def
      newPackage "b" $ do
        noopScript
        void defaultOutput
        void $ evalPackageOutput "a" def
  r <- runResolveM fakeNix $
    derivePackageId bucket "b"
  case r of
    Right b -> assertBool (ppShow b) False
    Left (WithCallStack (CircularDependencies _) _) -> return ()
    Left e -> assertBool (ppShow e) False

unit_Closure_Adds :: HasCallStack => IO ()
unit_Closure_Adds = do
  let
    Right bucket = newBucketM platformX86_64_linux $ do
      newPackage "a" $ do
        noopScript
        void defaultOutput
      let
        scope = do
          newPackage "src" $ do
            void defaultOutput
            noopScript
      newClosure "b" scope $ do
        noopScript
        void defaultOutput
        void $ evalPackageOutput "a" def
        void $ evalPackageOutput "src" def
  r <- runResolveM fakeNix $
    derivePackageId bucket "b"
  case r of
    Right b -> return ()
    Left e  -> assertBool (ppShow e) False

unit_Closure_Replaces :: HasCallStack => IO ()
unit_Closure_Replaces = do
  let
    Right bucket = newBucketM platformX86_64_linux $ do
      newPackage "a" $ do
        noopScript
        void defaultOutput
      let
        scope = do
          newPackage "a" $ do
            void $ setOutput "custom" SimpleOutput
            -- Sets other output. Must not resolve if scope not works
            noopScript
      newClosure "b" scope $ do
        noopScript
        void defaultOutput
        void $ evalPackageOutput "a" "custom"
  r <- runResolveM fakeNix $
    derivePackageId bucket "b"
  case r of
    Right b -> return ()
    Left e  -> assertBool (ppShow e) False
