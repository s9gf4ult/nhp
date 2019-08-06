module NHP.Monad where

import Control.Monad.Trans.RWS.Strict (RWST(..))
import Data.Text as T
import NHP.Imports
import NHP.Types

data DState = DState
  { nameCounter :: Integer
  } deriving Generic

data DWorld f = DWorld
  { getPackage :: PackageId -> f Package
  }

newtype DerivationM f a = DerivationM
  { unDerivation :: RWST (DWorld f) Script DState f a
  } deriving
  ( Functor, Applicative, Monad, MonadWriter Script
  , MonadState DState)

deriving instance (Monad f) => MonadReader (DWorld f) (DerivationM f)

instance MonadTrans DerivationM where
  lift ma = DerivationM $ lift ma

package :: (Monad f) => PackageId -> DerivationM f Package
package pkgid = do
  g <- asks getPackage
  lift $ g pkgid

newName :: (Monad f) => Text -> DerivationM f (Var a)
newName n = do
  st <- get
  let c = nameCounter st
  put $ st { nameCounter = succ c }
  return $ Var $ n <> (T.pack $ show c)
