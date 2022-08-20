{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Routes.Root where

import Servant.API
import Servant.API.Generic
import Servant.Links
import Lucid
import Data.UUID


import           Web.FormUrlEncoded          (FromForm)
import ServantLucid
import Routes.SessionAuth

newtype CreateHouseholdPayload = CreateHouseholdPayload
  { newHouseholdName :: Text
  } deriving Generic

instance FromForm CreateHouseholdPayload

data CreateChorePayload = CreateChorePayload
  { choreName :: Text
  , scheduleType :: Text
  , interval :: Maybe Int
  , days :: [Text]
  } deriving (Show, Generic)

instance FromForm CreateChorePayload


-- newtype ScheduleFormPayload =
--   ScheduleFormPayload {formType :: Text }
--   deriving Generic
--
-- instance FromForm ScheduleFormPayload

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
  , _households :: mode
      :- "households"
      :> AuthProtect "session-auth"
      :> Get '[HTML] (Html ())
  , _householdCreate :: mode
      :- "household-create"
      :> AuthProtect "session-auth"
      :> ReqBody '[FormUrlEncoded] CreateHouseholdPayload
      :> Post '[HTML] (Html ())
  , _householdLeave :: mode
      :- "household-leave"
      :> AuthProtect "session-auth"
      :> Capture "householdId" UUID
      :> Post '[HTML] (Html ())
  , _householdChores :: mode
      :- AuthProtect "session-auth"
      :> "household"
      :> Capture "householdName" Text
      :> "chores"
      :> Get '[HTML] (Html ())
  , _scheduleForm :: mode
      :- "schedule_form"
      :> QueryParam' '[Required, Strict] "scheduleType" Text
      :> Get '[HTML] (Html ())
  , _addWeekRow :: mode
      :- "add_week_row"
      :> Capture "row_id" Int
      :> Get '[HTML] (Html ())
  , _removeWeekRow :: mode
      :- "remove_week_row"
      :> Capture "row_id" Int
      :> Get '[HTML] (Html ())
  , _addMonthRow :: mode
      :- "add_month_row"
      :> Capture "row_id" Int
      :> Get '[HTML] (Html ())
  , _removeMonthRow :: mode
      :- "remove_month_row"
      :> Capture "row_id" Int
      :> Get '[HTML] (Html ())
  , _createChore :: mode
      :- AuthProtect "session-auth"
      :> "create_chore"
      :> ReqBody '[FormUrlEncoded] CreateChorePayload
      :> Post '[HTML] (Html ())
  , _landing :: mode
      :- Get '[HTML] (Html ())
  , _static :: mode
      :- "static"
      :> Raw
  } deriving Generic

rootLinks :: ChoreWheelApi (AsLink Link)
rootLinks = allFieldLinks

pingLink :: Link
pingLink = _ping rootLinks

loginLink :: Link
loginLink = _sessionLogin $ _session rootLinks
