module NHP.Monad.Types.NixBackend where

import           Control.Monad.Random
import qualified Data.Text.Lazy            as T
import           Filesystem.Path           as F
import           Filesystem.Path.CurrentOS as F
import           NHP.Imports
import           NHP.Types


fpFromText :: Text -> F.FilePath
fpFromText = F.fromText . T.toStrict

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
      let p = fpFromText "/nix/store/" F.</> fpFromText r
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
        return $ DerivationOutput p "SHA256" (T.toStrict r)
  in NixBackend storeAdd storeBin evalOut
