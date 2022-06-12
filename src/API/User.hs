{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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

newtype UserApi route = UserApi
  { _me :: route
      :- "me"
      :> Get '[JSON] User
  } deriving Generic

-------------------------------------------------------------------------------
-- Implementation

userApi' :: AuthResult JwtPayload -> ToServant UserApi (AsServerT App)
userApi' (Authenticated (JwtPayload i)) = genericServerT UserApi
  { _me = me $ UserId i }
userApi' _ = throwAll err401

userApi :: JwtPayload -> ToServant UserApi (AsServerT App)
userApi (JwtPayload i) = genericServerT UserApi
  { _me = me $ UserId i }

me :: UserM m => UserId -> m User
me = getUser
