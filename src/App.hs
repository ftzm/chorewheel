{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module App where

import Control.Monad.Reader
import Control.Monad.Except
import qualified Hasql.Pool as HP
import Servant.Server (ServerError)
import Servant.Auth.Server (JWTSettings)
import GHC.Generics

import Effect.Auth.Jwt
import Effect.Auth.Session
import Effect.User
import Effect.Household

data AppEnv = AppEnv
  { _pool :: HP.Pool
  , _jwtCfg :: JWTSettings
  } deriving Generic

newtype App a = App (ReaderT AppEnv (ExceptT ServerError IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadError ServerError)
  deriving AuthM via (AuthT App)
  deriving SessionAuthM via (SessionAuthT App)
  deriving UserM via (UserT App)
  deriving HouseholdM via (HouseholdT App)
