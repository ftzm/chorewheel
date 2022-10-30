{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Routes.Root where

import Data.Time.Calendar (Day)
import Data.UUID
import Lucid
import Servant.API
import Servant.Links

import Data.Generics.Internal.VL.Lens

import Chore
import Models
import Routes.SessionAuth
import ServantLucid
import Web.FormUrlEncoded (FromForm)

newtype CreateHouseholdPayload = CreateHouseholdPayload
  { newHouseholdName :: Text
  }
  deriving (Generic)

instance FromForm CreateHouseholdPayload

data CreateChorePayload = CreateChorePayload
  { choreName :: Text
  , scheduleType :: Text
  , interval :: Maybe Int
  , days :: [Text]
  }
  deriving (Show, Generic)

instance FromForm CreateChorePayload

data ChoreWheelApi mode = ChoreWheelApi
  { _ping ::
      mode
        :- "ping"
          :> Get '[PlainText] String
  , _session ::
      mode
        :- "session"
          :> NamedRoutes SessionAuth
  , _login ::
      mode
        :- "login"
          :> Header "Cookie" Text
          :> Get '[HTML] (Html ())
  , _home ::
      mode
        :- "home"
          :> AuthProtect "session-auth"
          :> Get '[HTML] (Html ())
  , _households ::
      mode
        :- "households"
          :> AuthProtect "session-auth"
          :> Get '[HTML] (Html ())
  , _householdCreate ::
      mode
        :- "household-create"
          :> AuthProtect "session-auth"
          :> ReqBody '[FormUrlEncoded] CreateHouseholdPayload
          :> Post '[HTML] (Html ())
  , _householdLeave ::
      mode
        :- "household-leave"
          :> AuthProtect "session-auth"
          :> Capture "householdId" UUID
          :> Post '[HTML] (Html ())
  , _householdChores ::
      mode
        :- AuthProtect "session-auth"
          :> "household"
          :> Capture "householdName" Text
          :> "chores"
          :> Get '[HTML] (Html ())
  , _scheduleForm ::
      mode
        :- "schedule_form"
          :> QueryParam' '[Required, Strict] "scheduleType" Text
          :> Get '[HTML] (Html ())
  , _addWeekRow ::
      mode
        :- "add_week_row"
          :> Capture "row_id" Int
          :> Get '[HTML] (Html ())
  , _removeWeekRow ::
      mode
        :- "remove_week_row"
          :> Capture "row_id" Int
          :> Get '[HTML] (Html ())
  , _addMonthRow ::
      mode
        :- "add_month_row"
          :> Capture "row_id" Int
          :> Get '[HTML] (Html ())
  , _removeMonthRow ::
      mode
        :- "remove_month_row"
          :> Capture "row_id" Int
          :> Get '[HTML] (Html ())
  , _createChore ::
      mode
        :- AuthProtect "session-auth"
          :> "create_chore"
          :> Capture "householdId" HouseholdId
          :> ReqBody '[FormUrlEncoded] CreateChorePayload
          :> Post '[HTML] (Html ())
  , _doChore ::
      mode
        :- AuthProtect "session-auth"
          :> "do_chore"
          :> Capture "householdId" HouseholdId
          :> Capture "choreId" ChoreId
          :> QueryParam' '[Required, Strict] "date" Day
          :> Post '[HTML] (Html ())
  , _landing ::
      mode
        :- Get '[HTML] (Html ())
  , _household ::
      mode
        :- AuthProtect "session-auth"
          :> "household"
          :> Capture "householdName" Text
          :> Get '[HTML] (Html ())
  , _static ::
      mode
        :- "static"
          :> Raw
  }
  deriving (Generic)

rootLinks :: ChoreWheelApi (AsLink URI)
rootLinks = allFieldLinks' (over #uriPath ("/" <>) . linkURI)
