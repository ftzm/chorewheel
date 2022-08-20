{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.SessionAuth where

import Servant.Server
import Servant.Server.Generic
import Control.Monad.Error.Class
import Web.Cookie

import Models
import Servant.API
import Effect.Auth.Session as Sess
import Routes.SessionAuth
import ApiUtil

sessionAuth
  :: MonadError ServerError m
  => SessionAuthM m
  => SessionAuth (AsServerT m)
sessionAuth = SessionAuth
  { _sessionLogin = sessionLogin
  , _sessionLogout = return sessionLogout
  }

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
