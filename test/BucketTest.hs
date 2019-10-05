module BucketTest (unit_DeriveEmpty) where

import           NHP.Imports
import           NHP.Monad
import           Test.Tasty
import           Test.Tasty.HUnit

emptpyBucket :: PackageBucket Identity
emptyBucket = runBucketM

unit_DeriveEmpty :: IO ()
unit_DeriveEmpty = error "FIXME: unit_DeriveEmpty not implemented"

-- unit_ResolveExpectations :: IO ()
-- unit_ResolveExpectations = do
--   let
--     pkg = derivePackageId ""
