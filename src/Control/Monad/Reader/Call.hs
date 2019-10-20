{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Monad.Reader.Call where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Generics.Product
import           GHC.TypeLits

class  MapResult a b f g | f a b -> g, g b a -> f where
  mapResult :: (a -> b) -> f -> g

instance MapResult a b a b where
  mapResult ab = ab
  {-# INLINE mapResult #-}

instance MapResult a b f g => MapResult a b (x -> f) (x -> g) where
  mapResult ab xf x = mapResult ab (xf x)
  {-# INLINE mapResult #-}

class RCall (r :: (* -> *) -> *) (m :: * -> *) v t
  | t -> v, t -> r, t -> m where
  rCall :: (r m -> v) -> t

instance (Monad m) => RCall r m (m a) (ReaderT (r m) m a) where
  rCall apply = do
    r <- ask
    let v = apply r
    lift v
  {-# INLINE rCall #-}

instance (RCall r m v t)
  => RCall r m (x -> v) (x -> t) where
  rCall apply = \x -> rCall @r @m @v @t (\rm -> apply rm x)
  {-# INLINE rCall #-}

call
  :: forall (fld :: Symbol) r m v t
  .  (RCall r m v t, HasField' fld (r m) v)
  => t
call = rCall @r @m @v @t (view $ field' @fld @(r m) @v)
{-# INLINE call #-}
