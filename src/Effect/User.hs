{-# LANGUAGE UndecidableInstances #-}

module Effect.User where

import Hasql.Session qualified as HS

import DB
import DB.User
import Models

class Monad m => UserM m where
  getUser :: UserId -> m User

newtype UserT m a = UserT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance (WithDb r m) => UserM (UserT m) where
  getUser i = runPool $ HS.statement i selectUser
