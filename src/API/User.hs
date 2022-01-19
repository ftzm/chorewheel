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

module API.User where

import Servant.Server
import Servant.Server.Generic
import Servant.API
import Servant.API.Generic
import Servant.Auth.Server

import Models
import App
import Effect.User

-------------------------------------------------------------------------------
-- API

data UserApi route = UserApi
  { _me :: route
      :- "me"
      :> Get '[JSON] User
  } deriving Generic

-------------------------------------------------------------------------------
-- Implementation

userApi :: (AuthResult JwtPayload) -> ToServant UserApi (AsServerT App)
userApi (Authenticated (JwtPayload i)) = genericServerT UserApi
  { _me = me $ UserId i }
userApi _ = throwAll err401

me :: UserM m => UserId -> m User
me i = getUser i