{-# LANGUAGE DataKinds #-}

module Server.SessionAuth where

import Control.Monad.Error.Class
import Servant.Server
import Servant.Server.Generic
import Web.Cookie

import ApiUtil
import Effect.Auth.Session as Sess
import Models
import Routes.Root
import Routes.SessionAuth
import Servant.API

sessionAuth ::
  MonadError ServerError m =>
  SessionAuthM m =>
  SessionAuth (AsServerT m)
sessionAuth =
  SessionAuth
    { sessionLogin = sessionLoginImpl
    , sessionLogout = return sessionLogoutImpl
    }

sessionLoginImpl ::
  MonadError ServerError m =>
  SessionAuthM m =>
  LoginForm ->
  m
    ( Headers
        '[ Header "Location" Text
         , Header "Set-Cookie" SetCookie
         ]
        NoContent
    )
sessionLoginImpl loginForm = do
  sessionToken <- justOrErr err401 =<< Sess.login username password
  let tokenString = encodeUtf8 $ unSessionToken sessionToken
  pure $
    addHeader (show rootLinks.home) $
      addHeader (defCookie "session-token" tokenString) NoContent
 where
  username = Username $ loginForm.username
  password = Password $ encodeUtf8 $ loginForm.password

sessionLogoutImpl ::
  Headers
    '[ Header "Location" Text
     , Header "Set-Cookie" SetCookie
     ]
    NoContent
sessionLogoutImpl =
  addHeader (show $ rootLinks.login) $ addHeader (removeCookie "session-token") NoContent
