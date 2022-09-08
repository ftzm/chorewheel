{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes          #-}

module Main where

import Prelude hiding (readFile, fromRight)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Hasql.Statement as S
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import qualified Hasql.Transaction as T
import qualified Hasql.Transaction.Sessions as TS
--import Data.Either
--import Data.Functor
import Data.Maybe
import Database.Postgres.Temp
import Data.ByteString(readFile)
import Control.Monad.Error.Class
import Data.Time.Clock
--import Control.Monad.IO.Class
--import Control.Monad.Reader
import qualified Data.Vector as V
import Data.UUID.V4
import Data.Time.Calendar (addDays)
import qualified Data.Set.NonEmpty as NESet

import Models
import Chore
import Schedule
import Schedule.Pattern
import Schedule.Primitives
import DB.User
import DB.Password
import qualified DB.RefreshToken as Ref
import Effect.Auth.Session
import DB.Household
import DB.Chore
import DB.Schedule
import DB.Util
import DB.SessionException()
import Participants
import Control.Monad.Catch

-------------------------------------------------------------------------------
-- Test utils

-- session c s =
  -- Session.run s c >>=
  -- either (fail . show) return

fromRight :: Either a b -> b
fromRight (Right a) = a
fromRight (Left _)  = error "unexpected Left"

withPool' :: Pool.Pool -> Session.Session a -> IO a
withPool' p s = Pool.use p s >>= either (fail . show) return

withPool :: Pool.Pool -> Session.Session a -> IO (Either Pool.UsageError a)
withPool = Pool.use

-- transaction' connection transaction =
  -- session connection (TS.transaction TS.RepeatableRead TS.Write transaction)

poolTransS :: Pool.Pool -> a -> S.Statement a b -> IO (Either Pool.UsageError b)
poolTransS pool input statement =
  withPool pool $ TS.transaction TS.RepeatableRead TS.Write $ do
                    output <- T.statement input statement
                    T.condemn
                    return output

withRollback :: Pool.Pool -> Session.Session a -> IO a
withRollback p s = Pool.use p protectedSession >>= either (fail . show) return
  where
    protectedSession =
      bracket (Session.sql "BEGIN") (const $ Session.sql "ROLLBACK") (const s)

rollBackOnError :: Pool.Pool -> Session.Session a -> IO a
rollBackOnError p s =
  Pool.use p protectedSession >>= either (fail . show) return
  where
    protectedSession = do
      result <- catchError (Right <$> s) $ \e -> pure $ Left e
      either (\e -> Session.sql "ROLLBACK" >> throwError e) return result

createTestPool :: DB -> IO Pool.Pool
createTestPool db = do
  let connStr = toConnectionString db
  pool <- Pool.acquire 10 connStr
  migration <- readFile "migration/init.sql"
  Pool.use pool $ Session.sql migration
  return pool

type SessionRunner = forall a. Session.Session a -> IO a

-------------------------------------------------------------------------------
main :: IO ()
main = void $ with $ \db -> do
  pool <- createTestPool db
  let runS = withRollback pool
  defaultMain $ tests runS pool

tests :: SessionRunner -> Pool.Pool -> TestTree
tests runS pool = testGroup "Tests" [unitTests runS pool]

assertCompletes :: Assertion
assertCompletes  = True @?= True

unitTests :: SessionRunner -> Pool.Pool -> TestTree
unitTests runS pool = testGroup "Query Tests"
  [ testCase "Do the thing" $ False @?= False
  , testCase "The other" $ (@?= ()) ()
  -- User
  , testCase "Insert user" $ do
      userId <- UserId <$> nextRandom
      runS $ Session.statement (User userId "test" "test") insertUser
      assertCompletes
  , testCase "Select user" $ runS $ do
      userId <- UserId <$> liftIO nextRandom
      Session.statement (User userId "test" "test") insertUser
      Session.statement userId selectUser
      liftIO assertCompletes
  -- Password
  , testCase "Password round trip id" $ runS $ do
      let pw = PasswordHash "test_password"
      userId <- UserId <$> liftIO nextRandom
      Session.statement (User userId "test" "test") insertUser
      Session.statement (userId, pw) insertPassword
      dbPw <- Session.statement userId selectPassword
      liftIO $ pw @?= dbPw
  , testCase "Password round trip username" $ runS $ do
      let pw = PasswordHash "test_password"
      userId <- UserId <$> liftIO nextRandom
      Session.statement (User userId "test" "test") insertUser
      Session.statement (userId, pw) insertPassword
      result <- Session.statement (Username "test") passwordInfoByUsername
      liftIO $ isJust result @?= True
  -- RefreshToken
  , testCase "Token round trip" $ runS $ do
      expiry <- addUTCTime 3000 <$> liftIO getCurrentTime
      now <- liftIO getCurrentTime
      let token = "test_token"
      userId <- UserId <$> liftIO nextRandom
      Session.statement (User userId "test" "test") insertUser
      Session.statement (userId, token, expiry) Ref.upsertToken
      Session.statement (userId, token, expiry) Ref.upsertToken --update
      dbUserId <- Session.statement (token, now) Ref.selectToken
      Session.statement userId Ref.deleteToken
      liftIO $ userId @?= fromJust dbUserId
  -- Session
  , testCase "Continue session fails when no session" $ do
      Pool.use pool $ Session.sql "BEGIN"
      result <- runReaderT (continueSessionImpl $ SessionToken "missingToken") pool
      Pool.use pool $ Session.sql "ROLLBACK"
      result @?= Nothing
  , testCase "Continue session finds user by token" $ do
      Pool.use pool $ Session.sql "BEGIN"
      userId <- UserId <$> liftIO nextRandom
      rollBackOnError pool $ Session.statement (User userId "test" "test") insertUser
      token <- rollBackOnError pool $ createSessionImpl userId
      result <- runReaderT (continueSessionImpl token) pool
      Pool.use pool $ Session.sql "ROLLBACK"
      result @?= Just userId
  , testCase "Kill Session removes token" $ do
      Pool.use pool $ Session.sql "BEGIN"
      userId <- UserId <$> liftIO nextRandom
      rollBackOnError pool $ Session.statement (User userId "test" "test") insertUser
      token <- rollBackOnError pool $ createSessionImpl userId
      userId' <- runReaderT (continueSessionImpl token) pool
      endResult <- flip runReaderT pool $ do
        killSessionImpl userId
        continueSessionImpl token
      Pool.use pool $ Session.sql "ROLLBACK"
      isJust userId' @?= True
      endResult @?= Nothing
  -- Household
  , testCase "Household round trip" $ runS $ do
      userId <- UserId <$> liftIO nextRandom
      Session.statement (User userId "test" "test") insertUser
      householdId <- HouseholdId <$> liftIO nextRandom
      Session.statement (Household householdId "home") insertHousehold
      Session.statement (householdId, userId) insertHouseholdMember
      result <- Session.statement userId getUserHouseholds
      liftIO $ V.head result @?= Household householdId "home"
      Session.statement (householdId, userId) removeHouseholdMember
      resultAfter <- Session.statement userId getUserHouseholds
      liftIO $ resultAfter @?= V.empty

      Session.statement householdId deleteEmptyHousehold
      liftIO assertCompletes
  -- Chore
  , testCase "Chore round trip" $ runS $ do
      userId <- UserId <$> liftIO nextRandom
      Session.statement (User userId "test" "test") insertUser
      householdId <- HouseholdId <$> liftIO nextRandom
      Session.statement (Household householdId "home") insertHousehold
      --Session.statement (householdId, userId') insertHouseholdMember
      choreId1 <- ChoreId <$> liftIO nextRandom
      choreId2 <- ChoreId <$> liftIO nextRandom
      Session.statement (householdId, choreId1, "sweep") insertChore
      Session.statement (householdId, choreId2, "vacuum") insertChore
      chores <- Session.statement householdId householdChores
      liftIO $ V.toList chores @?= [(choreId1, "sweep"), (choreId2, "vacuum")]
  -- Schedule
  , testCase "Chore+Schedule round trip" $ runS $ do
      userId <- UserId <$> liftIO nextRandom
      Session.statement (User userId "test" "test") insertUser
      householdId <- HouseholdId <$> liftIO nextRandom
      Session.statement (Household householdId "home") insertHousehold
      Session.statement (householdId, userId) insertHouseholdMember
      choreId1 <- ChoreId <$> liftIO nextRandom
      choreId2 <- ChoreId <$> liftIO nextRandom
      choreId3 <- ChoreId <$> liftIO nextRandom
      choreId4 <- ChoreId <$> liftIO nextRandom
      Session.statement (householdId, choreId1, "sweep") insertChore
      Session.statement (householdId, choreId2, "vacuum") insertChore
      Session.statement (householdId, choreId3, "dust") insertChore
      Session.statement (householdId, choreId4, "windows") insertChore
      today <- utctDay <$> liftIO getCurrentTime
      let flexdays = FlexDaysSS $ FlexDaysState (FlexDays 2) today
      let strict = StrictDaysSS $ StrictDaysState (StrictDays 2) today
      let weeklyPattern = Pattern (loadNESetUnsafe [(1, Mon), (2, Tue)]) 2
      let weekly = WeeklyPatternSS $ fromRight $ nextEligibleDayWeekly weeklyPattern 1 today
      let monthlyPattern = Pattern (loadNESetUnsafe [(1, DayOfMonth 10), (2, DayOfMonth 20)]) 2
      let monthly = MonthlyPatternSS $ fromRight $ nextEligibleDayMonthly monthlyPattern 1 today
      insertSchedule (choreId1, flexdays)
      insertSchedule (choreId2, strict)
      insertSchedule (choreId3, weekly)
      insertSchedule (choreId4, monthly)
      output1 <- Session.statement choreId1 getSchedule
      output2 <- Session.statement choreId2 getSchedule
      output3 <- Session.statement choreId3 getSchedule
      output4 <- Session.statement choreId4 getSchedule

      allOutput <- Session.statement householdId getFullChoresByHousehold
      --liftIO $ print allOutput
      liftIO $ V.length allOutput @?= 4

      Session.statement choreId1 deleteSchedule

      liftIO $ FlexDaysS (FlexDays 2) @?= output1
      liftIO $ StrictDaysS (StrictDays 2) @?= output2
      liftIO $ WeeklyPatternS weeklyPattern @?= output3
      liftIO $ MonthlyPatternS monthlyPattern @?= output4
  , testCase "resolve flex chore" $ do
      today <- utctDay <$> liftIO getCurrentTime

      choreId <- ChoreId <$> liftIO nextRandom
      userId <- UserId <$> liftIO nextRandom
      let flexScheduleState = FlexDaysSS $ FlexDaysState (FlexDays 2) today
      let flexChore = Chore choreId "flex" flexScheduleState Nothing Everyone
      let resolved1 = (length . fst) <$> (doChore flexChore $ Resolution (addDays 10 today) $ Completed userId)
      let resolved2 = (length . fst) <$> (doChore flexChore $ Resolution today $ Completed userId)
      resolved1 @?= Right 6
      resolved2 @?= Right 1

  , testCase "resolve strict chore" $ do
      today <- utctDay <$> liftIO getCurrentTime

      choreId <- ChoreId <$> liftIO nextRandom
      userId <- UserId <$> liftIO nextRandom
      let strictScheduleState = StrictDaysSS $  StrictDaysState (StrictDays 2) today
      let strictChore = Chore choreId "strict" strictScheduleState Nothing Everyone
      let resolved1 = (length . fst) <$> (doChore strictChore $ Resolution (addDays 10 today) $ Completed userId)
      let resolved2 = (length . fst) <$> (doChore strictChore $ Resolution today $ Completed userId)
      resolved1 @?= Right 6
      resolved2 @?= Right 1

  , testCase "resolve weekly chore" $ do
      today <- utctDay <$> liftIO getCurrentTime
      userId <- UserId <$> liftIO nextRandom

      choreId <- ChoreId <$> liftIO nextRandom
      let patternDays = [(1, Mon), (1, Wed), (1, Fri), (2, Tue), (2, Thu)]
      let weeklyPattern = Pattern (NESet.fromList $ fromList patternDays) 2
      let startingPatternState = fromRight $ nextEligibleDayWeekly weeklyPattern 0 today
      let startingScheduledDay = (\(PatternState _ pos) -> pos.day) startingPatternState
      let dayToComplete = addDays 14 startingScheduledDay
      let weeklyPatternState = WeeklyPatternSS startingPatternState
      let chore = Chore choreId "weekly" weeklyPatternState Nothing Everyone
      let output1 = (length . fst) <$> (doChore chore $ Resolution dayToComplete $ Completed userId)
      let output2 = (length . fst) <$> (doChore chore $ Resolution startingScheduledDay $ Completed userId)
      output1 @?= Right 6
      output2 @?= Right 1

  , testCase "resolve monthly chore" $ do
      today <- utctDay <$> liftIO getCurrentTime

      choreId <- ChoreId <$> liftIO nextRandom
      userId <- UserId <$> liftIO nextRandom
      let patternDays = [ (1, DayOfMonth 1)
                        , (1, DayOfMonth 15)
                        , (1, DayOfMonth 30)
                        , (2, DayOfMonth 10)
                        , (2, DayOfMonth 20)
                        ]
      let monthlyPattern = Pattern (NESet.fromList $ fromList patternDays) 2
      let startingPatternState = fromRight $ nextEligibleDayMonthly monthlyPattern 0 today
      let startingScheduledDay = (\(PatternState _ pos) -> pos.day) startingPatternState
      let dayToComplete = addDays 60 startingScheduledDay
      let weeklyPatternState = MonthlyPatternSS startingPatternState
      let chore = Chore choreId "weekly" weeklyPatternState Nothing Everyone
      let output1 = (length . fst) <$> (doChore chore $ Resolution dayToComplete $ Completed userId)
      let output2 = (length . fst) <$> (doChore chore $ Resolution startingScheduledDay $ Completed userId)
      output1 @?= Right 6
      output2 @?= Right 1
  , testCase "resolution round trips" $ runS $ do
      today <- utctDay <$> liftIO getCurrentTime
      choreId <- ChoreId <$> liftIO nextRandom
      userId <- UserId <$> liftIO nextRandom
      Session.statement (User userId "test" "test") insertUser
      householdId <- HouseholdId <$> liftIO nextRandom
      Session.statement (Household householdId "home") insertHousehold
      Session.statement (householdId, choreId, "sweep") insertChore
      let resolutions = V.fromList $ map ((choreId,) . flip Resolution (Completed userId) . flip addDays today) [0..5]
      Session.statement resolutions insertChoreEvents
      output <- Session.statement choreId getChoreEvents
      liftIO $ output @?= (V.reverse $ V.map snd resolutions)
  , testCase "getChoreEventsFrom" $ runS $ do
      today <- utctDay <$> liftIO getCurrentTime
      choreId <- ChoreId <$> liftIO nextRandom
      userId <- UserId <$> liftIO nextRandom
      Session.statement (User userId "test" "test") insertUser
      householdId <- HouseholdId <$> liftIO nextRandom
      Session.statement (Household householdId "home") insertHousehold
      Session.statement (householdId, choreId, "sweep") insertChore
      let resolutions = V.fromList $ map ((choreId,) . flip Resolution (Completed userId) . flip addDays today) [0..5]
      Session.statement resolutions insertChoreEvents
      output <- Session.statement (choreId, addDays 2 today) getChoreEventsFrom
      liftIO $ length output @?= 5
  , testCase "choreParticipantsRoundTrip" $ runS $ do
      householdId <- HouseholdId <$> liftIO nextRandom
      choreId <- ChoreId <$> liftIO nextRandom
      Session.statement (Household householdId "home") insertHousehold
      Session.statement (householdId, choreId, "sweep") insertChore

      let participants = Everyone

      insertParticipants (choreId, participants)
      output <- Session.statement choreId getChoreParticipants
      liftIO $ output @?= participants
  ]

-- patternTests :: TestTree
-- patternTests = testGroup "Pattern Tests"
--   [  testCase "" ]
