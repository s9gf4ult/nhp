module NHP.Error where

import           Control.Monad.Except
import           GHC.Generics         (Generic)
import           GHC.Stack


data WithCallStack e = WithCallStack
  { _error :: e
  , _stack :: CallStack
  } deriving (Generic, Functor, Show)

throwWithStack :: (HasCallStack, MonadError (WithCallStack e) m) => e -> m a
throwWithStack e = throwError $ WithCallStack e callStack
