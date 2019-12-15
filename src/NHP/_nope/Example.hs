module NHP.Example where

import NHP.Arrow

package :: PackageId -> Derivation f () Path
package pkid = Derivation $ \p () -> path p pkid


packageBinary :: PackageId -> Executable -> Derivation f () Path
packageBinary pid exec = Derivation $ \p () -> do
  pkg <- p pid
  let binP = pkg </> "bin" </> toPath exec
  return (binP, mempty)

fetchUri :: Text -> Derivation f () Path
fetchUri uri = Derivation $ \p () -> do
  curl <- p "curl"


example :: Derivation f () ()
example = proc () -> do
  p <- fetchUri "http://yoba" -< ()
  src <- unpack -< p
  deps <- myDeps ["yoba", "boba"]
  configure -< (src, deps)
  build -< src
  install -< src
