{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Routes.SessionAuth where

import Servant.API
import Servant.API.Generic
import           Web.FormUrlEncoded          (FromForm)
import Web.Cookie

import ApiUtil

data LoginForm = LoginForm
  { _username :: Text
  , _password :: Text
  } deriving Generic

instance FromForm LoginForm

data SessionAuth mode = SessionAuth
  { _sessionLogin :: mode
      :- "login"
      :> ReqBody '[FormUrlEncoded] LoginForm
      :> Post303 '[PlainText] '[Header "Set-Cookie" SetCookie] NoContent
  , _sessionLogout :: mode
      :- "logout"
      :> Get303 '[PlainText] '[Header "Set-Cookie" SetCookie] NoContent
  } deriving (Generic)
