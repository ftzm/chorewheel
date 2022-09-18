{-# LANGUAGE UndecidableInstances #-}

module Effect.Identifier where

import Data.UUID
import Data.UUID.V4

class Monad m => IdentifierM m where
  genId :: m UUID

newtype IdentifierT m a = IdentifierT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance (Monad m, MonadIO m) => IdentifierM (IdentifierT m) where
  genId = liftIO nextRandom
