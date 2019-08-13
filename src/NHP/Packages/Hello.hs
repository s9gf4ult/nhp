module NHP.Packages.Hello where

import           NHP.Import

hello :: (Monad f) => DerivationM f ()
hello = do
  let
    sha = error "Some sha"
    url = error "http://example.com/hello.tar.gz"
  src <- fetchUrl url sha
  tmp <- mkTmpDir
  unTar (pathLit src) tmp
  within tmp $ do
    out <- outPath def
    configure $ defaultConfigure out
    make []
    make ["install"]
