module Server.Root where

import Servant.Server
import Servant.Server.Generic
import Servant.Server.StaticFiles
import Lucid
import Control.Monad.Error.Class
import Data.UUID
import qualified Data.Text as T
import Data.Time.Calendar (Day, addDays)
import qualified Data.Map as M
import qualified Data.Map.Merge.Strict as MM
import Data.List

import Models
--import App
import Routes.Root
import Server.SessionAuth
import Effect.User
import Effect.Auth.Session
import Effect.Household
import Effect.Chore
import Effect.Identifier
import Effect.Time
import ApiUtil
import Page.Home
import Page.Login
import Page.Landing
import Page.Households
import Page.Chore
import Log
import Chore
import Schedule
import Schedule.Primitives
import Schedule.Pattern
import Participants
import Data.Time.Clock

choreWheelApi
  :: MonadError ServerError m
  => SessionAuthM m
  => UserM m
  => HouseholdM m
  => ChoreM m
  => LogM m
  => IdentifierM m
  => TimeM m
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
  , _addWeekRow = addWeekRowHandler
  , _removeWeekRow = removeWeekRowHandler
  , _addMonthRow = addMonthRowHandler
  , _removeMonthRow = removeMonthRowHandler
  , _createChore = createChoreHandler
  , _landing = landingHandler
  , _household = handleHousehold
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
  createHousehold u n
  households <- getHouseholds u
  return $ householdsFragment households

householdLeaveHandler
  :: MonadError ServerError m
  => HouseholdM m
  => UserId
  -> UUID
  -> m (Html ())
householdLeaveHandler u hId = do
  leaveHousehold u $ HouseholdId hId
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
      return $ choresPage householdId allChores

scheduleFormHandler :: Monad m => Text -> m (Html ())
scheduleFormHandler p = return $ case p of
  "unscheduled" -> span_ "Unscheduled"
  "strict" -> createStrictForm
  "flex" -> createFlexForm
  "weekly" -> createWeeklyForm 1
  "monthly" -> createMonthlyForm 1
  _ -> span_ "impossible"

addWeekRowHandler :: Monad m => Int -> m (Html ())
addWeekRowHandler i = pure $ addWeekRow i

removeWeekRowHandler :: Monad m => Int -> m (Html ())
removeWeekRowHandler i = pure $ removeWeekRow i

addMonthRowHandler :: Monad m => Int -> m (Html ())
addMonthRowHandler i = pure $ addMonthRow i

removeMonthRowHandler :: Monad m => Int -> m (Html ())
removeMonthRowHandler i = pure $ removeMonthRow i

toChore :: CreateChorePayload -> UUID -> Day -> Either Text Chore
toChore payload id' day =
  case payload.choreName of
    "" -> Left "empty chore name"
    _ -> Right $ Chore (ChoreId id') payload.choreName scheduleState Nothing Everyone
  where
    read :: Text -> Int
    read = either error id . readEither . T.unpack
    days :: [(Int, Int)]
    days = map (bimap read read . fmap (T.drop 1) . T.breakOn "-") payload.days
    schedule =
      case (payload.scheduleType, payload.interval) of
        ("unscheduled", _) -> UnscheduledS
        ("strict", Just interval) -> StrictDaysS $ StrictDays interval
        ("flex", Just interval) -> FlexDaysS $ FlexDays interval
        ("weekly",Just interval) ->
          either (const $ (error $ "invalid schedule: " <> (show payload))) id
           $ WeeklyPatternS <$> createPattern safeToEnum days interval
        ("monthly", Just interval) ->
          either (const $ (error $ "invalid schedule: " <> (show payload))) id
           $ MonthlyPatternS <$> createPattern mkDayOfMonth days interval
        _ -> error $ "invalid schedule: " <> show payload
    scheduleState = either (const $ error $ "Invalid pattern state") id $ nextEligibleDay day schedule

createChoreHandler :: (Monad m, TimeM m, IdentifierM m, ChoreM m) => UserId -> HouseholdId -> CreateChorePayload -> m (Html ())
createChoreHandler userId householdId payload = do
  today <- utctDay <$> now
  newUUID <- genId
  chore <- either (const $ error "Invalid chore") return $ toChore payload newUUID today
  _ <- saveChore userId householdId chore
  return $ toHtml @Text $ show chore


catMaybeFst :: [(Maybe a, b)] -> [(a, b)]
catMaybeFst xs = [ (a, b) | (Just a, b) <- xs]

mergeMapWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
mergeMapWith f =
  MM.merge MM.dropMissing MM.dropMissing (MM.zipWithMatched $ const f)

handleHousehold
  :: MonadError ServerError m
  => HouseholdM m
  => ChoreM m
  => TimeM m
  => LogM m
  => UserId
  -> Text
  -> m (Html ())
handleHousehold userId householdName = do
  household <- justOrErr err401 =<< householdFromName userId householdName
  cs <- getFullChores household.id'
  today <- utctDay <$> now
  let from = addDays (-10) today
  let until = addDays 15 from
  let pastDays = [from..(addDays (-1) today)]
  let futureDays = [(addDays 1 today)..until]
  let dayTuple = (pastDays, today, futureDays)
  --resolutions <- choreEvents userId household.id' from until
  rotationResolutions <-
    choreEvents userId household.id' (addDays (-365) today) today
  let input = flip map cs $ \c ->
        let stateDays =
              mapMaybe ssDay $ scheduleStateWindow from until $ c.schedule
            crs = fromMaybe [] $ M.lookup c.id' rotationResolutions
            rotation =
              map ScheduledCell $ genRotation crs household.members c.participants
        in ( c
           , map (flip lookup (zip stateDays rotation)) pastDays
           , lookup today (zip stateDays rotation)
           , map (flip lookup (zip stateDays rotation)) futureDays
           )
  return $ householdPage dayTuple input
