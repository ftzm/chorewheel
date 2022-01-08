{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module API where

import Servant.API
import Servant.Server
import Servant.Auth.Server
import Data.Proxy
import Control.Monad.Trans
import Web.Cookie
import Data.Time.Clock
import           Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Except

import Auth
import Models
import Log
import DB


-------------------------------------------------------------------------------
-- test that throwall works at all

type Protected'
    = "name"  :> Get '[JSON] String
 :<|> "email" :> Get '[JSON] String

protected' :: AuthResult User -> Server Protected'
protected' (Authenticated u) = return (show $ name u) :<|> return (show $ email u)
protected' _ = throwAll err401

-------------------------------------------------------------------------------

type JwtAuth = Auth '[JWT] User

type UserAPI
  = "users" :> Get '[JSON] [User]
  :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe String)
  :<|> "ping" :> Get '[PlainText] String
  :<|> "refresh" :> Get '[PlainText] (Headers '[Header "Set-Cookie" SetCookie] String)
  :<|> Auth '[JWT] User :> "test" :> Get '[PlainText] String

type SimpleUserAPI
  = (
    Auth '[JWT] User :> Protected
    :<|> "ping" :> Get '[PlainText] String
    :<|> "refresh" :> Get '[PlainText] (Headers '[Header "Set-Cookie" SetCookie] String)
    )

type Protected =
    "users" :> Get '[JSON] [User]
    :<|> "test" :> Get '[PlainText] String

protected :: (AuthResult User) -> ServerT Protected App
protected (Authenticated _) = allUsers' :<|> (return "oi")
--protected _ = throwError err401 :<|> throwError err401
protected _ = throwAll err401

protect :: MonadError ServerError m => (a -> m b) -> AuthResult a -> m b
protect r (Authenticated a) = r a
protect _ _ = throwError err401

protectAnon :: MonadError ServerError m => m b -> AuthResult a -> m b
protectAnon r (Authenticated _) = r
protectAnon _ _ = throwError err401

allUsers ::  GetUsers m => MonadLog m => m [User]
allUsers  = do
  users <- getUsers
  logDebug $ "Users got: " <> (show $ length users)
  return users

allUsers' ::  MonadGetUsers m => MonadLog m => m [User]
allUsers'  = do
  users <- getUsers'
  logDebug $ "Users got: " <> (show $ length users)
  return users

addUser ::  SetUser m => MonadLog m => User -> m (Maybe String)
addUser u = do
  userKey <- setUser u
  logDebug $ "Created user: " <> (show u)
  return $ Just userKey

ping :: MonadIO m => m String
ping = return "pong"


refresh :: MonadIO m => JWTSettings -> m (Headers '[Header "Set-Cookie" SetCookie] String)
refresh jwtCfg = do
  refreshToken <- liftIO genToken
  expiration <- addUTCTime (fromInteger 300) <$> liftIO getCurrentTime
  jwte <- liftIO $ makeJWT (User "charizard" "pokemon.awesome@hotmail.com") jwtCfg $ Just expiration
  liftIO $ putStrLn $ show jwte
  let jwt = case jwte of
        Left oops -> show oops
        Right x -> show x
  let cookie = def
          { setCookieName = "shabizzle"
          , setCookieValue = refreshToken
          }
  return $ addHeader cookie jwt

test :: MonadError ServerError m => AuthResult User -> m String
test (Authenticated _) = return "oi"
test _ = throwError err401

test' :: Monad m =>  User -> m String
test' _ = return "oi"

serverT :: MonadError ServerError m => MonadIO m => UserStore m => MonadLog m => JWTSettings -> ServerT UserAPI m
serverT jwtCfg = allUsers :<|> addUser :<|> ping :<|> (refresh jwtCfg) :<|> test

simpleServerT :: JWTSettings -> ServerT SimpleUserAPI App
simpleServerT jwtCfg  = protected :<|>  ping :<|> (refresh jwtCfg)


abstractApp :: JWTSettings -> (forall a. App a -> Handler a) -> Application
abstractApp jwtCfg f = serveWithContext (Proxy @SimpleUserAPI) ctx $ hoistServerWithContext (Proxy @SimpleUserAPI) (Proxy @'[CookieSettings, JWTSettings])  f $ simpleServerT jwtCfg
  where ctx = defaultCookieSettings :. jwtCfg :. EmptyContext

-- Create a concrete App monad stack and derive MonadGetUsers instance for it
-- using "GetUsersT"
newtype App a = App (ReaderT AppEnv (ExceptT ServerError IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadError ServerError)
  deriving MonadGetUsers via (GetUsersT App)
  deriving MonadLog via (ConsoleLogT App)
