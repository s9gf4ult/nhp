module NHP.Packages.Hello where

import           NHP.PackageImports

hello :: (Monad f) => DerivationM f ()
hello = do
  let
    sha = error "Some sha"
    url = error "http://example.com/hello.tar.gz"
  src <- fetchUrl url sha
  tmp <- mkTmpDir
  unTar (pathLit src) tmp
  within tmp $ do
    out <- defaultOutput
    configure $ defaultConfigure out
    make []
    make ["install"]
