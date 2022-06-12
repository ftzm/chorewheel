{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module API.Root where

import Servant.Server
import Servant.Server.Generic
import Servant.API
import Servant.API.Generic
import Servant.Auth.Server
--import Servant.HTML.Lucid
--import qualified Hasql.Pool as HP
--import Servant.Swagger
--import Data.Swagger
--import Data.Proxy
import Lucid
import Data.Text
import Data.Text.Encoding
import Control.Monad

import Models
import App
import API.Auth
import API.User
import API.Frontend.Home
import API.Frontend.Login
import API.Util
import Effect.Auth.Session
import Effect.User
import ServantLucid

---------------------------------------------------------------------------
-- test

newtype SubApi mode = SubApi { _route :: mode :- "test" :> Get '[PlainText] String } deriving Generic

subApi :: SubApi (AsServerT App)
subApi = SubApi { _route = return "shaboy" }

newtype RootApi mode = RootApi { _sub :: mode :- "sub" :> NamedRoutes SubApi } deriving Generic

rootApi :: ToServant RootApi (AsServerT App)
rootApi = genericServerT RootApi { _sub = subApi }

-- So it turns out that if you want to be able to `throwAll err401` on a whole
-- subroot, then that subroot needs to have the type `ToServant ApiType
-- (AsServer AppMonad)`. Subroots of /that/ subroot may however have the type
-- NamedRoutes SubApi.

---------------------------------------------------------------------------


data ChoreWheelApi mode = ChoreWheelApi
  { _ping :: mode
      :- "ping"
      :> Get '[PlainText] String
  , _auth :: mode
      :- "auth"
      :> NamedRoutes AuthApi
  --, _user :: mode
  --    :- "user"
  --    :> Auth '[JWT] JwtPayload
  --    :> ToServant UserApi AsApi
  , _root :: mode
      :- "root"
      :> ToServant RootApi AsApi
  , _login :: mode
      :- "login"
      :> Header "Cookie" Text
      :> Get '[HTML] (Html ())
  , _home :: mode
      :- "home"
      :> AuthProtect "session-auth"
      :> Get '[HTML] (Html ())
  } deriving Generic

protect :: ThrowAll r => (a -> r) -> AuthResult a -> r
protect r (Authenticated a) = r a
protect _ _ = throwAll err401

choreWheelApi :: ChoreWheelApi (AsServerT App)
choreWheelApi = ChoreWheelApi
  { _ping = return "pong"
  , _auth = authApi
  --, _user = protect userApi
  , _root = rootApi
  --, _login = \header -> return API.Frontend.Login.login
  , _login = handleLogin
  , _home = \userId -> return home
  }

handleLogin :: Maybe Text -> App (Html ())
handleLogin cookies = do
  let sessionToken =
        SessionToken . decodeUtf8 <$> getCookie cookies "session-token"
  userId <- join <$> traverse continue sessionToken
  case userId of
    Nothing -> return loginPage
    Just userId -> loggedInPage <$> getUser userId


--- apiProxy :: Proxy ChoreWheelApi
--- apiProxy = Proxy
---
--- -- | Swagger spec for Todo API.
--- todoSwagger :: Swagger
--- todoSwagger = toSwagger apiProxy
---   -- & info.title   .~ "Todo API"
---   -- & info.version .~ "1.0"
---   -- & info.description ?~ "This is an API that tests swagger integration"
---   -- & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")
