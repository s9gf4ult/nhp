module NHP.MonadicExample where

import           NHP.Monad

-- | Fetch fixed hash derivation. The path is known at eval state.
fetchUrl :: Url -> Sha256 -> DerivationM f Path
fetchUrl x = undefined

hello :: DerivationM f ()
hello = do
  src <- fetchUrl "http://example.com/hello.tar"
  dir <- unpack src
  within dir $ do
    configure
    build
    outDir <- setOutput "out"
    install outDir
