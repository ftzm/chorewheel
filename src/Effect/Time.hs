{-# LANGUAGE UndecidableInstances #-}

module Effect.Time where

import Data.Time.Clock

class Monad m => TimeM m where
  now :: m UTCTime

newtype TimeT m a = TimeT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance (Monad m, MonadIO m) => TimeM (TimeT m) where
  now = liftIO getCurrentTime
