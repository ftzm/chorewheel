{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Root where

import Servant.Server.Generic
import Lucid
import Data.Text (Text)
import Data.Text.Encoding
import Control.Monad

import Models
import App
import Routes.Root
import Server.SessionAuth
import Effect.User
import Effect.Auth.Session
import Effect.Household
import ApiUtil
import Page.Home
import Page.Login
import Page.Landing
import Page.Households

choreWheelApi :: ChoreWheelApi (AsServerT App)
choreWheelApi = ChoreWheelApi
  { _ping = return "pong"
  , _session = sessionAuth
  , _login = loginHandler
  , _home = homeHandler
  , _households = householdsHandler
  , _householdCreate = householdCreateHandler
  , _householdLeave = householdLeaveHandler
  , _landing = landingHandler
  }

loginHandler :: Maybe Text -> App (Html ())
loginHandler cookies = do
  let sessionToken =
        SessionToken . decodeUtf8 <$> getCookie cookies "session-token"
  uM <- join <$> traverse continue sessionToken
  case uM of
    Nothing -> return loginPage
    Just u -> loggedInPage <$> getUser u

homeHandler :: UserId -> App (Html ())
homeHandler _ = return home

landingHandler :: App (Html ())
landingHandler = return landingPage

householdsHandler :: UserId -> App (Html ())
householdsHandler u = do
  households <- getHouseholds u
  return $ householdsPage households

householdCreateHandler :: UserId -> CreateHouseholdPayload -> App (Html ())
householdCreateHandler u (CreateHouseholdPayload n) = do
  createHousehold u (Household n)
  households <- getHouseholds u
  return $ householdsFragment households

householdLeaveHandler :: UserId -> Int -> App (Html ())
householdLeaveHandler u hId = do
  leaveHousehold u $ HouseholdId $ fromIntegral hId
  households <- getHouseholds u
  return $ householdsFragment households
