{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}

module Models where

import GHC.Generics

import Servant.Auth.Server

import Data.Aeson
import Data.Text (Text)
import Data.Int (Int32)
import Control.Monad.Trans
import Data.ByteString

data User = User
  { name :: Text
  , email :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON User -- generated via Generic
instance FromJSON User -- generated via Generic
instance ToJWT User
instance FromJWT User

data JwtPayload = JwtPayload
  { userId :: Int32
  } deriving (Show, Eq, Generic)

instance ToJSON JwtPayload -- generated via Generic
instance FromJSON JwtPayload -- generated via Generic

instance ToJWT JwtPayload
instance FromJWT JwtPayload

class Monad m => GetUsers m where
  getUsers :: m [User]

class Monad m => SetUser m where
  setUser :: User -> m String

type UserStore m =
  ( GetUsers m
  , SetUser m
  )

-- Pass-through instance for transformers
instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , GetUsers m
  ) => GetUsers (t m) where
  getUsers = lift getUsers

newtype UserId = UserId {unUserId :: Int32} deriving (Show, Eq)

newtype Username = Username { unUsername :: Text } deriving Show

newtype RefreshToken = RefreshToken { unRefreshToken :: ByteString } deriving Show

newtype Jwt = Jwt { unJwt :: ByteString } deriving Show

newtype Password = Password { unPassword :: ByteString } deriving (Show, Eq)

newtype PasswordHash = PasswordHash { unPasswordHash :: Text } deriving (Show, Eq)
