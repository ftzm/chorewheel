{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Root where

import Servant.Server
import Servant.Server.Generic
import Lucid
import Data.Text
import Data.Text.Encoding
import Control.Monad

import Models
import App
import Routes.Root
import Server.SessionAuth
import Effect.User
import Effect.Auth.Session
import ApiUtil
import Page.Home
import Page.Login
import Page.Landing

choreWheelApi :: ChoreWheelApi (AsServerT App)
choreWheelApi = ChoreWheelApi
  { _ping = return "pong"
  , _session = sessionAuth
  , _login = loginHandler
  , _home = homeHandler
  , _landing = landingHandler
  }

loginHandler :: Maybe Text -> App (Html ())
loginHandler cookies = do
  let sessionToken =
        SessionToken . decodeUtf8 <$> getCookie cookies "session-token"
  userId <- join <$> traverse continue sessionToken
  case userId of
    Nothing -> return loginPage
    Just userId -> loggedInPage <$> getUser userId

homeHandler :: UserId -> App (Html ())
homeHandler userId = return home

landingHandler :: App (Html ())
landingHandler = return landingPage
