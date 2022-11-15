{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.Error.Class
import Data.ByteString (readFile)
import Data.Map qualified as M
import Data.Maybe
import Data.Set.NonEmpty qualified as NESet
import Data.Time.Calendar (addDays)
import Data.Time.Clock
import Data.UUID.V4
import Data.Vector qualified as V
import Database.Postgres.Temp
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import Hasql.Statement qualified as S
import Hasql.Transaction qualified as T
import Hasql.Transaction.Sessions qualified as TS
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (fromRight, readFile)

import Chore
import Control.Monad.Catch
import DB.Chore
import DB.Household
import DB.Password
import DB.RefreshToken qualified as Ref
import DB.Schedule
import DB.SessionException ()
import DB.User
import DB.Util
import Effect.Auth.Session
import Models
import Participants
import Schedule
import Schedule.Pattern
import Schedule.Primitives

-------------------------------------------------------------------------------
-- Test utils

getPatternPosition :: PatternState a -> PatternPosition
getPatternPosition (PatternState _ p) = p

-- Stolen from HUnit-Plus, which would not compile.

{- | Assert that the given computation throws an exception that
 matches a predicate.
-}
assertThrows ::
  (Exception e, Show e) =>
  (e -> Assertion) ->
  IO a ->
  Assertion
assertThrows check comp =
  let runComp =
        comp
          >> assertFailure
            "expected exception but computation finished normally"
   in handle check runComp

fromRight :: Show a => Either a b -> b
fromRight (Right a) = a
fromRight (Left a) = error $ show a

withPool' :: Pool.Pool -> Session.Session a -> IO a
withPool' p s = Pool.use p s >>= either throwM return

withPool :: Pool.Pool -> Session.Session a -> IO (Either Pool.UsageError a)
withPool = Pool.use

poolTransS :: Pool.Pool -> a -> S.Statement a b -> IO (Either Pool.UsageError b)
poolTransS pool input statement =
  withPool pool $ TS.transaction TS.RepeatableRead TS.Write $ do
    output <- T.statement input statement
    T.condemn
    return output

withRollback :: Pool.Pool -> Session.Session a -> IO a
withRollback p s = Pool.use p protectedSession >>= either throwM return
 where
  protectedSession =
    bracket_ (Session.sql "BEGIN") (Session.sql "ROLLBACK") s

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
  pool <- Pool.acquire 10 Nothing connStr
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
assertCompletes = True @?= True

unitTests :: SessionRunner -> Pool.Pool -> TestTree
unitTests runS pool =
  testGroup
    "Query Tests"
    [ testCase "Do the thing" $ False @?= False
    , testCase "The other" $ (@?= ()) ()
    , -- User
      testCase "Insert user" $ do
        userId <- UserId <$> nextRandom
        runS $ Session.statement (User userId "test" "test") insertUser
        assertCompletes
    , testCase "Select user" $ runS $ do
        userId <- UserId <$> liftIO nextRandom
        Session.statement (User userId "test" "test") insertUser
        Session.statement userId selectUser
        liftIO assertCompletes
    , -- Password
      testCase "Password round trip id" $ runS $ do
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
    , -- RefreshToken
      testCase "Token round trip" $ runS $ do
        expiry <- addUTCTime 3000 <$> liftIO getCurrentTime
        now <- liftIO getCurrentTime
        let token = "test_token"
        userId <- UserId <$> liftIO nextRandom
        Session.statement (User userId "test" "test") insertUser
        Session.statement (userId, token, expiry) Ref.upsertToken
        Session.statement (userId, token, expiry) Ref.upsertToken -- update
        dbUserId <- Session.statement (token, now) Ref.selectToken
        Session.statement userId Ref.deleteToken
        liftIO $ userId @?= fromJust dbUserId
    , -- Session
      testCase "Continue session fails when no session" $ do
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
    , -- Household
      testCase "Household round trip" $ runS $ do
        userId <- UserId <$> liftIO nextRandom
        Session.statement (User userId "test" "test") insertUser
        householdId <- HouseholdId <$> liftIO nextRandom
        let members = HouseholdMembers $ NESet.fromList $ fromList [User userId "test" "test"]
        Session.statement (householdId, "home") insertHousehold
        Session.statement (householdId, userId) insertHouseholdMember
        result <- Session.statement userId getUserHouseholds
        liftIO $ V.head result @?= Household householdId "home" members
        Session.statement (householdId, userId) removeHouseholdMember
        resultAfter <- Session.statement userId getUserHouseholds
        liftIO $ resultAfter @?= V.empty

        Session.statement householdId deleteEmptyHousehold
        liftIO assertCompletes
    , testCase "Household members round trip" $ runS $ do
        userId1 <- UserId <$> liftIO nextRandom
        userId2 <- UserId <$> liftIO nextRandom
        let user1 = User userId1 "test1" "test1"
        let user2 = User userId2 "test2" "test2"
        -- let members = HouseholdMembers $ NESet.fromList $ fromList [user1, user2]
        Session.statement (User userId1 "test1" "test1") insertUser
        Session.statement (User userId2 "test2" "test2") insertUser

        householdId <- HouseholdId <$> liftIO nextRandom
        Session.statement (householdId, "home") insertHousehold

        Session.statement (householdId, userId1) insertHouseholdMember
        Session.statement (householdId, userId2) insertHouseholdMember

        result <- Session.statement householdId getHouseholdMembers
        liftIO $ result @?= HouseholdMembers (NESet.fromList $ fromList [user1, user2])

        liftIO assertCompletes
    , -- Chore
      testCase "Chore round trip" $ runS $ do
        userId <- UserId <$> liftIO nextRandom
        Session.statement (User userId "test" "test") insertUser
        householdId <- HouseholdId <$> liftIO nextRandom
        -- let members = HouseholdMembers $ NESet.fromList $ fromList [User userId "test" "test"]
        Session.statement (householdId, "home") insertHousehold
        -- Session.statement (householdId, userId') insertHouseholdMember
        choreId1 <- ChoreId <$> liftIO nextRandom
        choreId2 <- ChoreId <$> liftIO nextRandom
        Session.statement (householdId, choreId1, "sweep") insertChore
        Session.statement (householdId, choreId2, "vacuum") insertChore
        chores <- Session.statement householdId householdChores
        liftIO $ V.toList chores @?= [(choreId1, "sweep"), (choreId2, "vacuum")]
    , testCase "No duplicate chore names" $ assertThrows (\ChoreNameExists -> assertCompletes) $ mapSqlError [("chore_name_unique", ChoreNameExists)] $ runS $ do
        userId <- UserId <$> liftIO nextRandom
        Session.statement (User userId "test" "test") insertUser
        householdId <- HouseholdId <$> liftIO nextRandom
        -- let members = HouseholdMembers $ NESet.fromList $ fromList [User userId "test" "test"]
        Session.statement (householdId, "home") insertHousehold
        -- Session.statement (householdId, userId') insertHouseholdMember
        choreId1 <- ChoreId <$> liftIO nextRandom
        choreId2 <- ChoreId <$> liftIO nextRandom
        Session.statement (householdId, choreId1, "sweep") insertChore
        Session.statement (householdId, choreId2, "sweep") insertChore
        liftIO assertCompletes
    , -- Schedule
      testCase "Chore+Schedule round trip" $ runS $ do
        userId1 <- UserId <$> liftIO nextRandom
        userId2 <- UserId <$> liftIO nextRandom
        userId3 <- UserId <$> liftIO nextRandom
        let user1 = User userId1 "test1" "test1"
        let user2 = User userId2 "test2" "test2"
        let user3 = User userId3 "test3" "test3"
        Session.statement user1 insertUser
        Session.statement user2 insertUser
        Session.statement user3 insertUser

        userId <- UserId <$> liftIO nextRandom
        Session.statement (User userId "test" "test") insertUser
        householdId <- HouseholdId <$> liftIO nextRandom
        -- let members = HouseholdMembers $ NESet.fromList $ fromList [user1, user2, user3]
        Session.statement (householdId, "home") insertHousehold
        Session.statement (householdId, userId) insertHouseholdMember
        choreId1 <- ChoreId <$> liftIO nextRandom
        choreId2 <- ChoreId <$> liftIO nextRandom
        choreId3 <- ChoreId <$> liftIO nextRandom
        choreId4 <- ChoreId <$> liftIO nextRandom
        Session.statement (householdId, choreId1, "sweep") insertChore
        Session.statement (householdId, choreId2, "vacuum") insertChore
        Session.statement (householdId, choreId3, "dust") insertChore
        Session.statement (householdId, choreId4, "windows") insertChore

        let participants = Some $ NESet.fromList $ fromList [userId1, userId2]
        insertParticipants (choreId1, participants)
        insertParticipants (choreId2, participants)
        insertParticipants (choreId3, participants)
        insertParticipants (choreId4, participants)

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
        liftIO $ print allOutput
        liftIO $ V.length allOutput @?= 4

        Session.statement choreId1 deleteSchedule

        liftIO $ FlexDaysS (FlexDays 2) @?= output1
        liftIO $ StrictDaysS (StrictDays 2) @?= output2
        liftIO $ WeeklyPatternS weeklyPattern @?= output3
        liftIO $ MonthlyPatternS monthlyPattern @?= output4
    , testCase "Chore update" $ runS $ do
        userId1 <- UserId <$> liftIO nextRandom
        userId2 <- UserId <$> liftIO nextRandom
        userId3 <- UserId <$> liftIO nextRandom
        -- let user1 = User userId1 "test1" "test1"
        -- let user2 = User userId2 "test2" "test2"
        -- let user3 = User userId3 "test3" "test3"
        -- let members = HouseholdMembers $ NESet.fromList $ fromList [user1, user2, user3]
        Session.statement (User userId1 "test1" "test1") insertUser
        Session.statement (User userId2 "test2" "test2") insertUser
        Session.statement (User userId3 "test3" "test3") insertUser

        userId <- UserId <$> liftIO nextRandom
        Session.statement (User userId "test" "test") insertUser
        householdId <- HouseholdId <$> liftIO nextRandom
        Session.statement (householdId, "home") insertHousehold
        Session.statement (householdId, userId) insertHouseholdMember
        choreId1 <- ChoreId <$> liftIO nextRandom
        choreId2 <- ChoreId <$> liftIO nextRandom
        choreId3 <- ChoreId <$> liftIO nextRandom
        choreId4 <- ChoreId <$> liftIO nextRandom
        Session.statement (householdId, choreId1, "a sweep") insertChore
        Session.statement (householdId, choreId2, "b vacuum") insertChore
        Session.statement (householdId, choreId3, "c dust") insertChore
        Session.statement (householdId, choreId4, "d windows") insertChore

        let participants = Some $ NESet.fromList $ fromList [userId1, userId2]
        insertParticipants (choreId1, participants)
        insertParticipants (choreId2, participants)
        insertParticipants (choreId3, participants)
        insertParticipants (choreId4, participants)

        today <- utctDay <$> liftIO getCurrentTime
        let flex = FlexDaysSS $ FlexDaysState (FlexDays 2) today
        let strict = StrictDaysSS $ StrictDaysState (StrictDays 2) today
        let weeklyPattern = Pattern (loadNESetUnsafe [(1, Mon), (2, Tue)]) 2
        let weekly = WeeklyPatternSS $ fromRight $ nextEligibleDayWeekly weeklyPattern 1 today
        let monthlyPattern = Pattern (loadNESetUnsafe [(1, DayOfMonth 10), (2, DayOfMonth 20)]) 2
        let monthly = MonthlyPatternSS $ fromRight $ nextEligibleDayMonthly monthlyPattern 1 today
        -- let chore1 = Chore choreId1 "sweep" flexdays Nothing participants
        -- let chore2 = Chore choreId2 "vacuum" strict Nothing participants
        -- let chore3 = Chore choreId3 "dust" weekly Nothing participants
        -- let chore4 = Chore choreId4 "windows" monthly Nothing participants
        insertSchedule (choreId1, flex)
        insertSchedule (choreId2, strict)
        insertSchedule (choreId3, weekly)
        insertSchedule (choreId4, monthly)

        let resolution = Resolution today $ Completed userId
        let (_, nextFlex) = fromRight $ resolveSchedule flex Nothing resolution
        let (_, nextStrict) = fromRight $ resolveSchedule strict Nothing resolution
        let (_, nextWeekly) = fromRight $ resolveSchedule weekly Nothing resolution
        let (_, nextMonthly) = fromRight $ resolveSchedule monthly Nothing resolution

        updateSchedule' (choreId1, nextFlex)
        updateSchedule' (choreId2, nextStrict)
        updateSchedule' (choreId3, nextWeekly)
        updateSchedule' (choreId4, nextMonthly)

        ss <- map (.schedule) . V.toList <$> Session.statement householdId getFullChoresByHousehold

        liftIO $ ss @?= [nextFlex, nextStrict, nextWeekly, nextMonthly]
    , testCase "resolve flex chore" $ do
        today <- utctDay <$> liftIO getCurrentTime

        choreId <- ChoreId <$> liftIO nextRandom
        userId <- UserId <$> liftIO nextRandom
        let flexScheduleState = FlexDaysSS $ FlexDaysState (FlexDays 2) today
        let flexChore = Chore choreId "flex" flexScheduleState Nothing Everyone
        let resolved1 = length . fst <$> (doChore flexChore $ Resolution (addDays 10 today) $ Completed userId)
        let resolved2 = length . fst <$> (doChore flexChore $ Resolution today $ Completed userId)
        resolved1 @?= Right 6
        resolved2 @?= Right 1
    , testCase "resolve strict chore" $ do
        today <- utctDay <$> liftIO getCurrentTime

        choreId <- ChoreId <$> liftIO nextRandom
        userId <- UserId <$> liftIO nextRandom
        let strictScheduleState = StrictDaysSS $ StrictDaysState (StrictDays 2) today
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
        let patternDays =
              [ (1, DayOfMonth 1)
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
        -- let members = HouseholdMembers $ NESet.fromList $ fromList [User userId "test" "test"]
        Session.statement (User userId "test" "test") insertUser
        householdId <- HouseholdId <$> liftIO nextRandom
        Session.statement (householdId, "home") insertHousehold
        Session.statement (householdId, choreId, "sweep") insertChore
        let resolutions = V.fromList $ map ((choreId,) . flip Resolution (Completed userId) . flip addDays today) [0 .. 5]
        Session.statement resolutions insertChoreEvents
        output <- Session.statement choreId getChoreEvents
        liftIO $ output @?= V.map snd resolutions
    , testCase "getChoreEventsFromTo" $ runS $ do
        today <- utctDay <$> liftIO getCurrentTime
        let until = addDays 10 today
        choreId <- ChoreId <$> liftIO nextRandom
        userId <- UserId <$> liftIO nextRandom
        -- let members = HouseholdMembers $ NESet.fromList $ fromList [User userId "test" "test"]
        Session.statement (User userId "test" "test") insertUser
        householdId <- HouseholdId <$> liftIO nextRandom
        Session.statement (householdId, "home") insertHousehold
        Session.statement (householdId, choreId, "sweep") insertChore
        let resolutions = V.fromList $ map ((choreId,) . flip Resolution (Completed userId) . flip addDays today) [0 .. 5]
        Session.statement resolutions insertChoreEvents
        output <- Session.statement (choreId, addDays 2 today, until) getChoreEventsFromTo
        liftIO $ length output @?= 5
    , testCase "getHouseholdChoreEventsFromTo" $ runS $ do
        today <- utctDay <$> liftIO getCurrentTime
        let until = addDays 10 today
        userId <- UserId <$> liftIO nextRandom
        choreId1 <- ChoreId <$> liftIO nextRandom
        choreId2 <- ChoreId <$> liftIO nextRandom
        let user = User userId "test" "test"
        Session.statement (user) insertUser
        householdId <- HouseholdId <$> liftIO nextRandom
        -- let members = HouseholdMembers $ NESet.fromList $ fromList [user]
        Session.statement (householdId, "home") insertHousehold
        Session.statement (householdId, choreId1, "sweep") insertChore
        Session.statement (householdId, choreId2, "vacuum") insertChore
        let chore1Resolutions = map ((choreId1,) . flip Resolution (Completed userId) . flip addDays today) [0 .. 5]
        let chore2Resolutions = map ((choreId2,) . flip Resolution (Completed userId) . flip addDays today) [0 .. 5]
        let resolutions = V.fromList $ chore1Resolutions ++ chore2Resolutions
        Session.statement resolutions insertChoreEvents
        output <- Session.statement (householdId, addDays 2 today, until) getHouseholdChoreEventsFromTo
        print output
        liftIO $ length (M.foldr (++) [] output) @?= 8
    , testCase "choreParticipantsRoundTrip Everyone" $ runS $ do
        -- userId <- UserId <$> liftIO nextRandom
        -- let user = User userId "test" "test"
        -- let members = HouseholdMembers $ NESet.fromList $ fromList [user]
        householdId <- HouseholdId <$> liftIO nextRandom
        choreId <- ChoreId <$> liftIO nextRandom
        Session.statement (householdId, "home") insertHousehold
        Session.statement (householdId, choreId, "sweep") insertChore

        let participants = Everyone

        insertParticipants (choreId, participants)
        output <- Session.statement choreId getChoreParticipants
        liftIO $ output @?= participants
    , testCase "choreParticipantsRoundTrip Some" $ runS $ do
        userId1 <- UserId <$> liftIO nextRandom
        userId2 <- UserId <$> liftIO nextRandom
        userId3 <- UserId <$> liftIO nextRandom
        let user1 = User userId1 "test1" "test1"
        let user2 = User userId2 "test2" "test2"
        let user3 = User userId3 "test3" "test3"
        -- let members = HouseholdMembers $ NESet.fromList $ fromList [user1, user2, user3]
        Session.statement (user1) insertUser
        Session.statement (user2) insertUser
        Session.statement (user3) insertUser

        householdId <- HouseholdId <$> liftIO nextRandom
        choreId <- ChoreId <$> liftIO nextRandom
        Session.statement (householdId, "home") insertHousehold
        Session.statement (householdId, choreId, "sweep") insertChore

        let participants = Some $ NESet.fromList $ fromList [userId1, userId2]

        insertParticipants (choreId, participants)
        output <- Session.statement choreId getChoreParticipants
        liftIO $ output @?= participants
    , testCase "calculate participant rotation" $ do
        today <- utctDay <$> liftIO getCurrentTime
        userId1 <- UserId <$> liftIO nextRandom
        userId2 <- UserId <$> liftIO nextRandom
        userId3 <- UserId <$> liftIO nextRandom
        userId4 <- UserId <$> liftIO nextRandom
        userId5 <- UserId <$> liftIO nextRandom
        let user2 = User userId2 "name2" "email2"
        let user3 = User userId3 "name3" "email3"
        let user4 = User userId4 "name4" "email4"
        let user5 = User userId5 "name5" "email5"
        let resolutions =
              [ Resolution today Lapsed
              , Resolution today $ Completed userId3
              , Resolution today Skipped
              , Resolution today Skipped
              , Resolution today $ Completed userId1
              , Resolution today $ Completed userId2
              ]
        let householdMembers = HouseholdMembers $ NESet.fromList $ fromList [user2, user3, user4, user5]
        let someParticipants = Some $ NESet.fromList $ fromList [userId2, userId3, userId4]

        let resultSome = genRotation resolutions householdMembers someParticipants
        let resultEveryone = genRotation resolutions householdMembers Everyone
        let resultNone = genRotation resolutions householdMembers None

        (take 3 resultSome) @?= [user4, user3, user2]
        (take 2 $ drop 2 $ resultEveryone) @?= [user3, user2]
        resultNone @?= []
    , testCase "generating weekly states forwards and backwards yields same results" $ do
        today <- utctDay <$> liftIO getCurrentTime
        let patternDays = [(1, Mon), (1, Wed), (1, Fri), (2, Tue), (2, Thu)]
        let weeklyPattern = Pattern (NESet.fromList $ fromList patternDays) 2
        let weeklyPatternState = fromRight $ nextEligibleDayWeekly weeklyPattern 0 today
        let forward10 = weeklyPatternState :| take 9 (futureStatesWeekly weeklyPatternState)
        let reverse10 = last forward10 :| take 9 (pastStatesWeekly $ last forward10)
        map getPatternPosition (toList forward10) @?= map getPatternPosition (reverse (toList reverse10))
    , testCase "generating monthly states forwards and backwards yields same results" $ do
        today <- utctDay <$> liftIO getCurrentTime
        let patternDays =
              [ (1, DayOfMonth 1)
              , (1, DayOfMonth 15)
              , (1, DayOfMonth 30)
              , (2, DayOfMonth 10)
              , (2, DayOfMonth 20)
              ]
        let monthlyPattern = Pattern (NESet.fromList $ fromList patternDays) 2
        let monthlyPatternState = fromRight $ nextEligibleDayMonthly monthlyPattern 0 today
        let forward10 = monthlyPatternState :| take 9 (futureStatesMonthly monthlyPatternState)
        let reverse10 = last forward10 :| take 9 (pastStatesMonthly $ last forward10)
        map getPatternPosition (toList forward10) @?= map getPatternPosition (reverse (toList reverse10))
    ]

-- patternTests :: TestTree
-- patternTests = testGroup "Pattern Tests"
--   [  testCase "" ]
