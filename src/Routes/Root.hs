{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Routes.Root where

import Servant.API
import Servant.API.Generic
import Servant.Links
import Lucid
import Data.Text

import ServantLucid
import Models
import App
import Routes.SessionAuth

data ChoreWheelApi mode = ChoreWheelApi
  { _ping :: mode
      :- "ping"
      :> Get '[PlainText] String
  , _session :: mode
      :- "session"
      :> NamedRoutes SessionAuth
  , _login :: mode
      :- "login"
      :> Header "Cookie" Text
      :> Get '[HTML] (Html ())
  , _home :: mode
      :- "home"
      :> AuthProtect "session-auth"
      :> Get '[HTML] (Html ())
  , _landing :: mode
      :- Get '[HTML] (Html ())
  } deriving Generic

rootLinks :: ChoreWheelApi (AsLink Link)
rootLinks = allFieldLinks

pingLink :: Link
pingLink = _ping rootLinks

loginLink :: Link
loginLink = _sessionLogin $ _session rootLinks
