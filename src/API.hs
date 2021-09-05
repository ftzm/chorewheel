{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module API where

import Servant.API
import Servant.Server
import Data.Proxy

import Models
import Log

type UserAPI
  = "users" :> Get '[JSON] [User]
  :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe String)

allUsers ::  GetUsers m => MonadLog m => m [User]
allUsers = do
  users <- getUsers
  logDebug $ "Users got: " <> (show $ length users)
  return users

addUser ::  SetUser m => MonadLog m => User -> m (Maybe String)
addUser user = do
  userKey <- setUser user
  logDebug $ "Created user: " <> (show user)
  return $ Just userKey

serverT :: UserStore m => MonadLog m => ServerT UserAPI m
serverT = allUsers :<|> addUser

abstractApp :: UserStore m => MonadLog m => (forall a. m a -> Handler a) -> Application
abstractApp f = serve (Proxy @UserAPI) $ hoistServer (Proxy @UserAPI) f serverT
