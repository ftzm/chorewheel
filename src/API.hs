{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Prelude hiding (break, drop)
import GHC.Generics
import Data.Aeson
import Servant.API
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic
import Servant.Auth.Server
import Data.Proxy
import Control.Monad.Trans
import Web.Cookie
import Data.Time.Clock
import           Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Except
import Data.Text (Text, stripPrefix)
import Data.Text.Encoding
import Database.Persist.Sql
import qualified Data.ByteString as BS

import Crypto.KDF.BCrypt (hashPassword)

--import Auth
import Models
import Log
import DB

-------------------------------------------------------------------------------


data ChorewheelApi route = ChorewheelApi
  { _userApi :: route
      :- Auth '[JWT] JwtPayload
      :> ToServant UserApi AsApi
  , _ping :: route
      :- "ping"
      :> Get '[PlainText] String
  , _login :: route
      :- "login"
      :> Header "Authorization" Text
      :> Post '[PlainText] (Headers '[Header "Set-Cookie" SetCookie] String)
  , _refresh :: route
      :- "refresh"
      :> Header "Cookie" Text
      :> Get '[PlainText] (Headers '[Header "Set-Cookie" SetCookie] String)
  } deriving (Generic)

data UserApi route = UserApi
  { users :: route
      :- "users"
      :> Get '[JSON] [User]
  , me :: route
      :- "me"
      :> Get '[JSON] (Maybe User)
  } deriving (Generic)

userApi :: (AuthResult JwtPayload) -> ToServant UserApi (AsServerT App)
userApi (Authenticated jwt) = genericServerT UserApi
  { users = return []
  , me = meHandler jwt
  }
userApi _ = throwAll err401

appServer' :: JWTSettings -> ChorewheelApi (AsServerT App)
appServer' jwtCfg = ChorewheelApi
  { _userApi = \authData -> (userApi authData)
  , _ping = return "pong"
  , _login = login jwtCfg
  , _refresh = refresh jwtCfg
  }

login :: WithDb r m => MonadError ServerError m => RefreshToken m => MonadIO m => JWTSettings -> Maybe Text -> m (Headers '[Header "Set-Cookie" SetCookie] String)
login jwtCfg header = do
  hashed' :: BS.ByteString <- liftIO $ hashPassword 10 ("test" :: BS.ByteString)
  liftIO $ print hashed'
  let authStringO = (stripPrefix "Basic " =<< header)
  case authStringO of
    Nothing -> throwError err401
    Just authString -> do
      userKeyO <- validateBasicAuth authString
      case userKeyO of
        Nothing -> throwError err401
        Just userKey -> do
          refreshToken <- createRefreshToken userKey
          jwt <- createJWT jwtCfg userKey
          let cookie = def
                   { setCookieName = "Refresh-Token"
                   , setCookieValue = refreshToken
                   }
          return $ addHeader cookie jwt

meHandler :: MonadGetUsers m => JwtPayload -> m (Maybe User)
meHandler jwt = getUser $ userId jwt

protect :: MonadError ServerError m => (a -> m b) -> AuthResult a -> m b
protect r (Authenticated a) = r a
protect _ _ = throwError err401
--
--protectAnon :: MonadError ServerError m => m b -> AuthResult a -> m b
--protectAnon r (Authenticated _) = r
--protectAnon _ _ = throwError err401

allUsers ::  GetUsers m => MonadLog m => m [User]
allUsers  = do
  u <- getUsers
  logDebug $ "Users got: " <> (show $ length u)
  return u

allUsers' ::  MonadGetUsers m => MonadLog m => m [User]
allUsers'  = do
  u <- getUsers'
  logDebug $ "Users got: " <> (show $ length u)
  return u

addUser ::  SetUser m => MonadLog m => User -> m (Maybe String)
addUser u = do
  userKey <- setUser u
  logDebug $ "Created user: " <> (show u)
  return $ Just userKey
data JwtPayload = JwtPayload
  { userId :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON JwtPayload -- generated via Generic
instance FromJSON JwtPayload -- generated via Generic
instance ToJWT JwtPayload
instance FromJWT JwtPayload

getCookie :: Maybe Text -> BS.ByteString -> Maybe BS.ByteString
getCookie cookies n = lookup n =<< parseCookies . encodeUtf8 <$> cookies

createJWT :: MonadIO m => JWTSettings -> Key DbUser -> m String
createJWT jwtCfg userId' = do
  expiration <- addUTCTime (fromInteger 300) <$> liftIO getCurrentTime
  let userId'' :: Int = fromIntegral $ fromSqlKey userId'
  jwte <- liftIO $ makeJWT (JwtPayload userId'') jwtCfg $ Just expiration
  case jwte of
    Left joseError -> error $ show joseError
    Right x -> pure $ show x

refresh :: MonadError ServerError m => RefreshToken m => MonadIO m => JWTSettings -> Maybe Text -> m (Headers '[Header "Set-Cookie" SetCookie] String)
refresh jwtCfg cookies = do
  refreshToken <- maybe
    (throwError err401)
    pure $ getCookie cookies "Refresh-Token"
  (newRefreshToken, userId') <- (redeemRefreshToken refreshToken) >>= (maybe
    ((liftIO $ putStrLn "token not in db") >> throwError err401)) pure
  jwt <- createJWT jwtCfg userId'
  let cookie = def
          { setCookieName = "Refresh-Token"
          , setCookieValue = newRefreshToken
          }
  pure $ addHeader cookie jwt

p :: Proxy (ToServantApi ChorewheelApi)
p = genericApi (Proxy :: Proxy ChorewheelApi)

abstractApp :: JWTSettings -> (forall a. App a -> Handler a) -> Application
--abstractApp jwtCfg f = genericServeTWithContext p ctx $ hoistServerWithContext p ctxProxy f
abstractApp jwtCfg f = genericServeTWithContext f server ctx
  where
    ctx = defaultCookieSettings :. jwtCfg :. EmptyContext
    server :: ChorewheelApi (AsServerT App)
    server = appServer' jwtCfg

-- Create a concrete App monad stack and derive MonadGetUsers instance for it
-- using "GetUsersT"
newtype App a = App (ReaderT AppEnv (ExceptT ServerError IO) a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadError ServerError)
  deriving MonadGetUsers via (GetUsersT App)
  deriving MonadLog via (ConsoleLogT App)
  deriving RefreshToken via (RefreshTokenT App)

-- TODO: Investigate how to use this to convert internal errors to servant errors
appToHandler :: ConnectionPool -> App a -> Handler a
appToHandler pool (App m) = do
  val <- liftIO $ runExceptT $ runReaderT m $ AppEnv pool
  case val of
    Left e -> throwError e
    Right s -> return s
