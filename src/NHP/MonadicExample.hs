module NHP.MonadicExample where

import           NHP.Imports
import           NHP.Monad
import           NHP.Types

-- | Fetch fixed hash derivation. The path is known at eval state.
fetchUrl :: Monad f => Url -> Sha256 -> DerivationM f Path
fetchUrl url sha = do
  (pkg, outId) <- evalDerivation $ deriveFetchUrl url sha
  getPackageOutput pkg outId

deriveFetchUrl :: Url -> Sha256 -> DerivationM f OutputId
deriveFetchUrl url sha = do
  curl <- packageBin "curl" def "curl"
  let outId = def
  outPath <- outFixedPath outId sha
  writeScript $ do
    callBin curl [strLit "-o", strLit $ unPath outPath, strLit $ urlText url]
    checkHash outPath sha
  return outId

hello :: DerivationM f ()
hello = do
  src <- fetchUrl "http://example.com/hello.tar"
  dir <- unpack src
  within dir $ do
    configure
    build
    outDir <- setOutput "out"
    install outDir
