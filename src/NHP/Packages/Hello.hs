module NHP.Packages.Hello where

import           NHP.PackageImports

hello :: (Monad f, HasCallStack) => BucketM f ()
hello = do
  let
    src = do
      newPackage "source" $ do
        let
          sha = error "Some sha"
          url = error "http://example.com/hello.tar.gz"
        deriveFetchUrl url sha
  newClosure "hello" src $ do
    src <- evalPackageOutput "source" def
    tmp <- mkTmpDir
    unTar (pathLit src) tmp
    within tmp $ do
      out <- defaultOutput
      configure $ defaultConfigure out
      make []
      make ["install"]
