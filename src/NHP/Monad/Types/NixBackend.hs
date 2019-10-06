module NHP.Monad.Types.NixBackend where

import           Control.Monad.Random
import qualified Data.Text                 as T
import           Filesystem.Path           as F
import           Filesystem.Path.CurrentOS as F
import           NHP.Imports
import           NHP.Types

-- | The nix-store backend
data NixBackend f = NixBackend
  { _storeAdd       :: HasCallStack => F.FilePath -> f Path
  -- ^ Store derivation or other path in the Nix store
  , _storeAddBinary :: HasCallStack => ByteString -> f Path
  -- ^ Store binary data in the store and return the path
  , _evalOutputPath
    :: HasCallStack
    => Derivation
    -- ^ Derivation with empty outputs
    -> OutputId
    -> Output
    -> f DerivationOutput
  }


fakeNix :: NixBackend IO
fakeNix =
  let
    rndText :: IO Text
    rndText = evalRandIO $ do
      fmap T.pack $ sequence $ replicate 10 $ do
        liftRand $ randomR ('a', 'z')
    rndPath :: IO F.FilePath
    rndPath = do
      r <- rndText
      let p = F.fromText "/nix/store/" F.</> F.fromText r
      return p
    storeAdd _fp = Path <$> rndText
    storeBin _bin = Path <$> rndText
    evalOut _drv _outId = \case
      SimpleOutput -> do
        p <- rndPath
        return $ DerivationOutput p "" ""
      FixedHashOutput _sha -> do
        p <- rndPath
        r <- rndText
        return $ DerivationOutput p "SHA256" r
  in NixBackend storeAdd storeBin evalOut
