{-# LANGUAGE UndecidableInstances #-}

module Effect.Identifier where

import Data.UUID
import Data.UUID.V4

class Monad m => IdentifierM m where
  genId :: m UUID

instance (Monad m, MonadIO m) => IdentifierM m where
  genId = liftIO nextRandom
