{-# LANGUAGE UndecidableInstances #-}

module Log where

import Control.Monad.Identity

class Monad m => MonadLog m where
  logDebug :: Show msg => msg -> m ()

-- Pass-through instance for transformers
instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadLog m
  ) => MonadLog (t m) where
  logDebug = lift . logDebug


-- | Newtype for disabling logging
newtype NoLoggingT m a
  = NoLoggingT { runNoLoggingT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving (MonadTrans) via IdentityT

instance Monad m => MonadLog (NoLoggingT m) where logDebug _ = pure ()


-- Transformer for logging to Console
newtype ConsoleLogT m a
  = ConsoleLogT { runConsoleLogT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving (MonadTrans) via IdentityT

-- Instance using fast-logger to print to console
instance MonadIO m => MonadLog (ConsoleLogT m) where
  logDebug msg = ConsoleLogT $ liftIO $ print msg
