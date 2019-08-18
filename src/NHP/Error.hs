module NHP.Error where

import NHP.Imports

data WithCallStack e = WithCallStack
  { _error     :: e
  , _stack :: CallStack
  } deriving (Generic, Functor)

throwWithStack :: (HasCallStack, MonadError (WithCallStack e) m) => e -> m a
throwWithStack e = throwError $ WithCallStack e callStack
