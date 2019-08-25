module NHP.Packages.Hello where

import           NHP.PackageImports

hello :: (Monad f, HasCallStack) => BucketM f ()
hello = do
  newPackage "hello-source" $ do
    let
      sha = error "Some sha"
      url = error "http://example.com/hello.tar.gz"
    deriveFetchUrl url sha
  newPackage "hello" $ do
    src <- evalPackageOutput "hello-source" def
    tmp <- mkTmpDir
    unTar (pathLit src) tmp
    within tmp $ do
      out <- defaultOutput
      configure $ defaultConfigure out
      make []
      make ["install"]
