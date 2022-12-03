{-# LANGUAGE DataKinds #-}

module Routes.SessionAuth where

import Servant.API
import Web.Cookie
import Web.FormUrlEncoded (FromForm)

import ApiUtil

data LoginForm = LoginForm
  { username :: Text
  , password :: Text
  }
  deriving (Generic)

instance FromForm LoginForm

data SessionAuth mode = SessionAuth
  { sessionLogin ::
      mode
        :- "login"
          :> ReqBody '[FormUrlEncoded] LoginForm
          :> Post303 '[PlainText] '[Header "Set-Cookie" SetCookie] NoContent
  , sessionLogout ::
      mode
        :- "logout"
          :> Get303 '[PlainText] '[Header "Set-Cookie" SetCookie] NoContent
  }
  deriving (Generic)
