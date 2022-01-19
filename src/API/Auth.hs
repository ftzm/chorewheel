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
import Control.Monad.IO.Class
import Data.ByteString.Base64 (decodeLenient)

import Models
import App
import Effect.Auth
import API.Util

-------------------------------------------------------------------------------
-- API

data AuthApi route = AuthApi
  { _login :: route
      :- "login"
      :> Header "Authorization" Text
      :> Post '[PlainText] (Headers '[Header "Set-Cookie" SetCookie] Text)
  , _refresh :: route
      :- "refresh"
      :> Header "Cookie" Text
      :> Get '[PlainText] (Headers '[Header "Set-Cookie" SetCookie] Text)
  } deriving (Generic)

-------------------------------------------------------------------------------
-- Implementation

authApi :: ToServant AuthApi (AsServerT App)
authApi = genericServerT AuthApi
  { _login = login'
  , _refresh = refresh
  }

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
  (Headers '[Header "Set-Cookie" SetCookie] Text)
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

login' ::
  MonadIO m =>
  MonadError ServerError m =>
  AuthM m =>
  Maybe Text ->
  m (Headers '[Header "Set-Cookie" SetCookie] Text)
login' header = do
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
