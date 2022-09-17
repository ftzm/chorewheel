{-# LANGUAGE UndecidableInstances #-}

module Effect.Identifier where

import Data.Time.Clock

class Monad m => IdentifierM m where
  now :: m UTCTime

instance (Monad m, MonadIO m) => IdentifierM m where
  now = liftIO getCurrentTime
