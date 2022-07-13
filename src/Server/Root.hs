{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Root where

import Servant.Server
import Servant.Server.Generic
import Servant.Server.StaticFiles
import Lucid
import Data.Text (Text)
import Data.Text.Encoding
import Control.Monad
import Control.Monad.Error.Class

import Models
--import App
import Routes.Root
import Server.SessionAuth
import Effect.User
import Effect.Auth.Session
import Effect.Household
import Effect.Chore
import ApiUtil
import Page.Home
import Page.Login
import Page.Landing
import Page.Households
import Page.Chore

choreWheelApi
  :: MonadError ServerError m
  => SessionAuthM m
  => UserM m
  => HouseholdM m
  => ChoreM m
  => ChoreWheelApi (AsServerT m)
choreWheelApi = ChoreWheelApi
  { _ping = return "pong"
  , _session = sessionAuth
  , _login = loginHandler
  , _home = homeHandler
  , _households = householdsHandler
  , _householdCreate = householdCreateHandler
  , _householdLeave = householdLeaveHandler
  , _householdChores = householdChoresHandler
  , _scheduleForm = scheduleFormHandler
  , _landing = landingHandler
  , _static = serveDirectoryWebApp "static"
  }

loginHandler
  :: MonadError ServerError m
  => SessionAuthM m
  => UserM m
  =>  Maybe Text
  -> m (Html ())
loginHandler cookies = do
  let sessionToken =
        SessionToken . decodeUtf8 <$> getCookie cookies "session-token"
  uM <- join <$> traverse continue sessionToken
  case uM of
    Nothing -> return loginPage
    Just u -> loggedInPage <$> getUser u

homeHandler :: Monad m => UserId -> m (Html ())
homeHandler _ = return home

landingHandler :: Monad m => m (Html ())
landingHandler = return landingPage

householdsHandler
  :: MonadError ServerError m
  => HouseholdM m
  => UserId
  -> m (Html ())
householdsHandler u = do
  households <- getHouseholds u
  return $ householdsPage households

householdCreateHandler
  :: MonadError ServerError m
  => HouseholdM m
  => UserId
  -> CreateHouseholdPayload
  -> m (Html ())
householdCreateHandler u (CreateHouseholdPayload n) = do
  createHousehold u (Household n)
  households <- getHouseholds u
  return $ householdsFragment households

householdLeaveHandler
  :: MonadError ServerError m
  => HouseholdM m
  => UserId
  -> Int
  -> m (Html ())
householdLeaveHandler u hId = do
  leaveHousehold u $ HouseholdId $ fromIntegral hId
  households <- getHouseholds u
  return $ householdsFragment households

householdChoresHandler
  :: MonadError ServerError m
  => HouseholdM m
  => ChoreM m
  => UserId
  -> Text
  -> m (Html ())
householdChoresHandler u n = do
  householdM <- householdIdFromName u n
  case householdM of
    Nothing -> throwError err401
    Just householdId -> do
      allChores <- getFullChores householdId
      return $ choresPage allChores

scheduleFormHandler
  :: Monad m
  => UserId
  -> Text
  -> m (Html ())
scheduleFormHandler _ p = return $ case p of
  "strict" -> createStrictForm
  "flex" -> createFlexForm
  "weekly" -> span_ "weekly"
  "monthly" -> span_ "monthly"
  _ -> span_ "impossible"
