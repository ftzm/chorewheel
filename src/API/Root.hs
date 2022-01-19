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

module API.Root where

--import Servant.Server
import Servant.Server.Generic
import Servant.API
import Servant.API.Generic
import Servant.Auth.Server
--import qualified Hasql.Pool as HP
--import Servant.Swagger
--import Data.Swagger
--import Data.Proxy

import Models
import App
import API.Auth
import API.User

data ChoreWheelApi mode = ChoreWheelApi
  { _ping :: mode
      :- "ping"
      :> Get '[PlainText] String
  , _auth :: mode
      :- "auth"
      :> ToServant AuthApi AsApi
  , _user :: mode
      :- "user"
      :> Auth '[JWT] JwtPayload
      :> ToServant UserApi AsApi
  } deriving Generic

choreWheelApi :: ChoreWheelApi (AsServerT App)
choreWheelApi = ChoreWheelApi
  { _ping = return "pong"
  , _auth = authApi
  , _user = userApi
  }


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
