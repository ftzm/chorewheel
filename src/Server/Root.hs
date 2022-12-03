module Server.Root where

import Control.Monad.Error.Class
import Data.List
import Data.Map qualified as M
import Data.Map.Merge.Strict qualified as MM
import Data.Text qualified as T
import Data.Time.Calendar (Day, addDays)
import Data.UUID
import Data.Vector qualified as V
import Lucid
import Servant.Server
import Servant.Server.Generic
import Servant.Server.StaticFiles

import Models

-- import App

import ApiUtil
import Chore
import Data.Time.Clock
import Effect.Auth.Session
import Effect.Chore
import Effect.Household
import Effect.Identifier
import Effect.Time
import Effect.User
import Log
import Page.Chore
import Page.Home
import Page.Households
import Page.Landing
import Page.Login
import Participants
import Routes.Root
import Schedule
import Schedule.Pattern
import Schedule.Primitives
import Server.SessionAuth

choreWheelApi ::
  MonadError ServerError m =>
  SessionAuthM m =>
  UserM m =>
  HouseholdM m =>
  ChoreM m =>
  LogM m =>
  IdentifierM m =>
  TimeM m =>
  ChoreWheelApi (AsServerT m)
choreWheelApi =
  ChoreWheelApi
    { ping = return "pong"
    , session = sessionAuth
    , login = loginHandler
    , home = homeHandler
    , households = householdsHandler
    , householdCreate = householdCreateHandler
    , householdLeave = householdLeaveHandler
    , householdChores = householdChoresHandler
    , scheduleForm = scheduleFormHandler
    , addWeekRow = addWeekRowHandler
    , removeWeekRow = removeWeekRowHandler
    , addMonthRow = addMonthRowHandler
    , removeMonthRow = removeMonthRowHandler
    , createChore = createChoreHandler
    , doChore = handleDoChore
    , undoChore = handleUndoChore
    , landing = landingHandler
    , household = handleHousehold
    , static = serveDirectoryWebApp "static"
    }

loginHandler ::
  MonadError ServerError m =>
  SessionAuthM m =>
  UserM m =>
  Maybe Text ->
  m (Html ())
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

householdsHandler ::
  MonadError ServerError m =>
  HouseholdM m =>
  UserId ->
  m (Html ())
householdsHandler u = do
  households <- getHouseholds u
  return $ householdsPage households

householdCreateHandler ::
  MonadError ServerError m =>
  HouseholdM m =>
  UserId ->
  CreateHouseholdPayload ->
  m (Html ())
householdCreateHandler u (CreateHouseholdPayload n) = do
  createHousehold u n
  households <- getHouseholds u
  return $ householdsFragment households

householdLeaveHandler ::
  MonadError ServerError m =>
  HouseholdM m =>
  UserId ->
  UUID ->
  m (Html ())
householdLeaveHandler u hId = do
  leaveHousehold u $ HouseholdId hId
  households <- getHouseholds u
  return $ householdsFragment households

householdChoresHandler ::
  MonadError ServerError m =>
  HouseholdM m =>
  ChoreM m =>
  UserId ->
  Text ->
  m (Html ())
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
      ("weekly", Just interval) ->
        fromRight (error $ "invalid schedule: " <> show payload) $
          WeeklyPatternS <$> createPattern safeToEnum days interval
      ("monthly", Just interval) ->
        fromRight (error $ "invalid schedule: " <> show payload) $
          MonthlyPatternS <$> createPattern mkDayOfMonth days interval
      _ -> error $ "invalid schedule: " <> show payload
  scheduleState =
    fromRight (error "Invalid pattern state") $
      nextEligibleDay day schedule

createChoreHandler :: (Monad m, TimeM m, IdentifierM m, ChoreM m) => UserId -> HouseholdId -> CreateChorePayload -> m (Html ())
createChoreHandler userId householdId payload = do
  today <- utctDay <$> now
  newUUID <- genId
  chore <- either (const $ error "Invalid chore") return $ toChore payload newUUID today
  _ <- saveChore userId householdId chore
  return $ toHtml @Text $ show chore

catMaybeFst :: [(Maybe a, b)] -> [(a, b)]
catMaybeFst xs = [(a, b) | (Just a, b) <- xs]

mergeMapWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
mergeMapWith f =
  MM.merge MM.dropMissing MM.dropMissing (MM.zipWithMatched $ const f)

handleHousehold ::
  MonadError ServerError m =>
  HouseholdM m =>
  ChoreM m =>
  TimeM m =>
  LogM m =>
  UserId ->
  Text ->
  m (Html ())
handleHousehold userId householdName = do
  household <- justOrErr err401 =<< householdFromName userId householdName
  cs <- getFullChores household.id'
  today <- utctDay <$> now
  let from = addDays (-10) today
  let until = addDays 15 from
  resolutions <- allChoreEvents userId household.id' from until
  rotationResolutions <-
    allChoreEvents userId household.id' (addDays (-365) today) until
  let pastDays = [from .. (addDays (-1) today)]
  let futureDays = [(addDays 1 today) .. until]
  let dayTuple = (pastDays, today, futureDays)
  let input = flip map cs $ \c ->
        let stateDays =
              mapMaybe ssDay $ scheduleStateWindow from until $ c.schedule
            crs = fromMaybe [] $ M.lookup c.id' rotationResolutions
            rotation =
              map ScheduledCell $ genRotation crs household.members c.participants
            choreResolutions = M.findWithDefault [] c.id' resolutions
            getCell d =
              (ResolutionCell <$> find ((d ==) . (.day)) choreResolutions)
                <|> lookup d (zip stateDays rotation)
         in ( c
            , map getCell pastDays
            , getCell today
            , map getCell futureDays
            )
  return $ householdPage household.id' dayTuple input

getChoreRotation ::
  ChoreM m =>
  TimeM m =>
  UserId ->
  Household ->
  Chore ->
  m [User]
getChoreRotation userId household chore = do
  today <- utctDay <$> now
  rs <-
    toList
      <$> choreEvents userId household.id' chore.id' (addDays (-365) today) today
  pure $ genRotation rs household.members chore.participants

handleDoChore ::
  MonadError ServerError m =>
  HouseholdM m =>
  ChoreM m =>
  TimeM m =>
  LogM m =>
  UserId ->
  HouseholdId ->
  ChoreId ->
  Day ->
  m (Html ())
handleDoChore userId householdId choreId day = do
  oldChore <- getFullChore choreId
  resolveChore userId householdId oldChore $ Resolution day $ Completed userId
  getGridRow userId householdId choreId

getGridRow ::
  MonadError ServerError m =>
  ChoreM m =>
  HouseholdM m =>
  TimeM m =>
  UserId ->
  HouseholdId ->
  ChoreId ->
  m (Html ())
getGridRow userId householdId choreId = do
  c <- getFullChore choreId
  household <- justOrErr err401 =<< getHousehold userId householdId
  rotation <- getChoreRotation userId household c
  today <- utctDay <$> now
  let from = addDays (-10) today
  let until = addDays 15 from
  let pastDays = [from .. (addDays (-1) today)]
  let futureDays = [(addDays 1 today) .. until]
  let dayTuple = (pastDays, today, futureDays)
  resolutions <- choreEvents userId household.id' choreId from until
  let input = buildGridRowInput from until today pastDays futureDays c rotation resolutions
  return $ gridRow household.id' dayTuple input

buildGridRowInput :: Day -> Day -> Day -> [Day] -> [Day] -> Chore -> [User] -> V.Vector Resolution -> (Chore, [Maybe CellType], Maybe CellType, [Maybe CellType])
buildGridRowInput from until today pastDays futureDays chore rotation resolutions =
  let stateDays =
        mapMaybe ssDay $ scheduleStateWindow from until $ chore.schedule
      getCell d =
        let resolutionCell = ResolutionCell <$> find ((d ==) . (.day)) resolutions
            rotationCell = ScheduledCell <$> lookup d (zip stateDays rotation)
         in resolutionCell <|> rotationCell
   in ( chore
      , map getCell pastDays
      , getCell today
      , map getCell futureDays
      )

handleUndoChore ::
  MonadError ServerError m =>
  HouseholdM m =>
  ChoreM m =>
  TimeM m =>
  LogM m =>
  UserId ->
  HouseholdId ->
  ChoreId ->
  Day ->
  m (Html ())
handleUndoChore userId householdId choreId day = do
  -- delete chore resolution on day
  deleteChoreEvent userId choreId day

  getLatestChoreEvent userId householdId choreId
    >>= mapM_ \Resolution{day = latestDay} ->
      when (latestDay < day) $ do
        chore <- getFullChore choreId
        let reversed = reverseUntil latestDay chore.schedule
        mapM_ (updateSchedule userId choreId) reversed

  getGridRow userId householdId choreId
