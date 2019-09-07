module BucketTest (unit_DeriveEmpty) where

import           Test.Tasty
import           Test.Tasty.HUnit

emptyBucket :: PackageBucket Identity
emptyBucket = error "FIXME: emptyBucket not implemented"

unit_DeriveEmpty :: IO ()
unit_DeriveEmpty = error "FIXME: unit_DeriveEmpty not implemented"

-- unit_ResolveExpectations :: IO ()
-- unit_ResolveExpectations = do
--   let
--     pkg = derivePackageId ""
