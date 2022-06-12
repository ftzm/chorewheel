{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module API.Auth where

import Prelude hiding (break, drop)
import Servant.API
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic
import Servant.Auth.Server
import Data.Text (Text, stripPrefix, break, drop)
import Data.Text.Encoding
import Control.Monad.Error.Class
import Control.Monad
import Data.ByteString.Base64 (decodeLenient)
import           Web.FormUrlEncoded          (FromForm)
import Data.Function


import Models
import App
import Effect.Auth.Jwt as JWT
import Effect.Auth.Session as Sess
import API.Util

-------------------------------------------------------------------------------
-- API

data LoginForm = LoginForm
  { _username :: Text
  , _password :: Text
  } deriving Generic

instance FromForm LoginForm

data AuthApi route = AuthApi
  -- JWT
  { _login :: route
      :- "login"
      :> Header "Authorization" Text
      :> Post '[PlainText] (Headers '[Header "Set-Cookie" SetCookie] Text)
  , _refresh :: route
      :- "refresh"
      :> Header "Cookie" Text
      :> Get '[PlainText] (Headers '[Header "Set-Cookie" SetCookie] Text)
  , _cookieLogin :: route
      :- "cookie_login"
      :> ReqBody '[FormUrlEncoded] LoginForm
      :> Post '[PlainText] (Headers '[ Header "Set-Cookie" SetCookie
                                     , Header "Set-Cookie" SetCookie]
                             NoContent)
  -- Session
  , _sessionLogin :: route
      :- "session"
      :> ReqBody '[FormUrlEncoded] LoginForm
      :> Post303 '[PlainText] '[Header "Set-Cookie" SetCookie] NoContent
  , _sessionLogout :: route
      :- "session_logout"
      :> Get303 '[PlainText] '[Header "Set-Cookie" SetCookie] NoContent

  } deriving (Generic)

-------------------------------------------------------------------------------
-- Implementation

authApi :: AuthApi (AsServerT App)
authApi = AuthApi
  { _login = API.Auth.login
  , _refresh = refresh
  , _cookieLogin = cookieLogin
  , _sessionLogin = sessionLogin
  , _sessionLogout = return sessionLogout
  }

-- JWT

parseBasicAuthHeader :: Text -> Maybe (Username, Password)
parseBasicAuthHeader header = extractComponents <$> content
  where
    content = stripPrefix "Basic " header
    extractComponents c =
      let
        decoded = decodeUtf8 $ decodeLenient $ encodeUtf8 c
        (x, y) = break (==':') decoded
      in (Username x, Password . encodeUtf8 . drop 1 $ y)

createResponse ::
  RefreshToken ->
  Jwt ->
  Headers '[Header "Set-Cookie" SetCookie] Text
createResponse r j = addHeader cookie $ decodeUtf8 $ unJwt j
  where cookie = defCookie "Refresh-Token" $ unRefreshToken r

login ::
  MonadError ServerError m =>
  AuthM m =>
  Maybe Text ->
  m (Headers '[Header "Set-Cookie" SetCookie] Text)
login header = do
  let components = parseBasicAuthHeader =<< header
  result <- join <$> traverse (uncurry loginForTokens) components
  justOrErr err401 $ uncurry createResponse <$> result

refresh ::
  MonadError ServerError m =>
  AuthM m =>
  Maybe Text ->
  m (Headers '[Header "Set-Cookie" SetCookie] Text)
refresh cookies = do
  let tokenO = RefreshToken <$> getCookie cookies "Refresh-Token"
  result <- join <$> traverse refreshTokens tokenO
  justOrErr err401 $ uncurry createResponse <$> result

cookieLogin ::
  MonadError ServerError m =>
  AuthM m =>
  LoginForm ->
  m (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie ] NoContent)
cookieLogin loginForm = do
  (refresh, jwt) <- justOrErr err401 =<< loginForTokens username password
  pure $ NoContent
    & addHeader (defCookie "Refresh-Token" (unRefreshToken refresh))
    & addHeader (defCookie "Auth-Token" (unJwt jwt))
  where
    username = Username $ _username loginForm
    password = Password $ encodeUtf8 $ _password loginForm

-- session

sessionLogin ::
  MonadError ServerError m =>
  SessionAuthM m =>
  LoginForm ->
  m (Headers '[ Header "Location" Text
              , Header "Set-Cookie" SetCookie ] NoContent)
sessionLogin loginForm = do
  sessionToken <- justOrErr err401 =<< Sess.login username password
  let tokenString = encodeUtf8 $ unSessionToken sessionToken
  pure $ addHeader "/home"
       $ addHeader (defCookie "session-token" tokenString) NoContent
  where
    username = Username $ _username loginForm
    password = Password $ encodeUtf8 $ _password loginForm

sessionLogout :: Headers '[ Header "Location" Text
                          , Header "Set-Cookie" SetCookie] NoContent
sessionLogout =
  addHeader "/login" $ addHeader (removeCookie "session-token") NoContent
