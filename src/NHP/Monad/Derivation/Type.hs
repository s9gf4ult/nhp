module NHP.Monad.Derivation.Type where

newtype DerivationM script m a = DerivationM
  (ReaderT (DMethods script m) m a)

data DMethods = DMethods
  { }
