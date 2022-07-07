{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes          #-}

module Main where

import Prelude hiding (readFile)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Hasql.Statement as S
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import qualified Hasql.Transaction as T
import qualified Hasql.Transaction.Sessions as TS
--import Data.Either
import Data.Functor
import Data.Maybe
import Database.Postgres.Temp
import Data.ByteString(readFile)
import Control.Monad.Error.Class
import Data.Time.Clock
import Control.Monad.IO.Class
import Data.Vector hiding (map)
import qualified Data.Set as Set


import Models
import Chore
import Schedule
import Schedule.Pattern
import Schedule.Primitives
import DB.User
import DB.Password
import DB.RefreshToken
import DB.Household
import DB.Chore
import DB.Schedule

-------------------------------------------------------------------------------
-- Test utils

-- session c s =
  -- Session.run s c >>=
  -- either (fail . show) return

withPool' :: Pool.Pool -> Session.Session a -> IO a
withPool' p s = Pool.use p s >>= either (fail . show) return

withPool :: Pool.Pool -> Session.Session a -> IO (Either Pool.UsageError a)
withPool  = Pool.use

-- transaction' connection transaction =
  -- session connection (TS.transaction TS.RepeatableRead TS.Write transaction)

poolTransS :: Pool.Pool -> a -> S.Statement a b -> IO (Either Pool.UsageError b)
poolTransS pool input statement =
  withPool pool $ TS.transaction TS.RepeatableRead TS.Write $ do
                    output <- T.statement input statement
                    T.condemn
                    return output

withRollback :: Pool.Pool -> Session.Session a -> IO a
withRollback p s =
  Pool.use p protectedSession >>= either (fail . show) return
  where
    protectedSession = do
      Session.sql "BEGIN"
      result <- catchError (Right <$> s) $ \e -> pure $ Left e
      Session.sql "ROLLBACK"
      either throwError return result

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
  defaultMain $ tests runS

tests :: SessionRunner -> TestTree
tests runS = testGroup "Tests" [queryTests runS]

assertCompletes :: Assertion
assertCompletes  = True @?= True

queryTests :: SessionRunner -> TestTree
queryTests runS = testGroup "Query Tests"
  [ testCase "Do the thing" $ False @?= False
  -- User
  , testCase "Insert user" $ do
      runS $ Session.statement (User "test" "test") insertUser
      assertCompletes
  , testCase "Select user" $ runS $ do
      newId <- Session.statement (User "test" "test") insertUser
      Session.statement newId selectUser
      liftIO assertCompletes
  -- Password
  , testCase "Password round trip id" $ runS $ do
      let pw = PasswordHash "test_password"
      newId <- Session.statement (User "test" "test") insertUser
      Session.statement (newId, pw) insertPassword
      dbPw <- Session.statement newId selectPassword
      liftIO $ pw @?= dbPw
  , testCase "Password round trip username" $ runS $ do
      let pw = PasswordHash "test_password"
      newId <- Session.statement (User "test" "test") insertUser
      Session.statement (newId, pw) insertPassword
      result <- Session.statement (Username "test") passwordInfoByUsername
      liftIO $ isJust result @?= True
  -- RefreshToken
  , testCase "Token round trip" $ runS $ do
      expiry <- addUTCTime 3000 <$> liftIO getCurrentTime
      now <- liftIO getCurrentTime
      let token = "test_token"
      originalId <- Session.statement (User "test" "test") insertUser
      Session.statement (originalId, token, expiry) upsertToken
      Session.statement (originalId, token, expiry) upsertToken --update
      dbUserId <- Session.statement (token, now) selectToken
      Session.statement originalId deleteToken
      liftIO $ originalId @?= fromJust dbUserId
  -- Household
  , testCase "Insert household, membership, and get" $ runS $ do
      userId' <- Session.statement (User "test" "test") insertUser
      householdId <- Session.statement (Household "home") insertHousehold
      Session.statement (householdId, userId') insertHouseholdMember
      result <- Session.statement userId' getUserHouseholds
      liftIO $ Data.Vector.head result @?= (householdId, Household "home")
      liftIO assertCompletes
  -- Chore
  , testCase "Chore rounde trip" $ runS $ do
      Session.statement (User "test" "test") insertUser
      householdId <- Session.statement (Household "home") insertHousehold
      --Session.statement (householdId, userId') insertHouseholdMember
      Session.statement (householdId, Chore "sweep") insertChore
      Session.statement (householdId, Chore "vacuum") insertChore
      chores <- Session.statement householdId householdChores
      liftIO $ map snd (toList chores) @?= [Chore "sweep", Chore "vacuum"]
  -- Schedule
  , testCase "Schedule round trip" $ runS $ do
      userId' <- Session.statement (User "test" "test") insertUser
      householdId <- Session.statement (Household "home") insertHousehold
      Session.statement (householdId, userId') insertHouseholdMember
      choreId1 <- Session.statement (householdId, Chore "sweep") insertChore
      choreId2 <- Session.statement (householdId, Chore "vacuum") insertChore
      choreId3 <- Session.statement (householdId, Chore "dust") insertChore
      choreId4 <- Session.statement (householdId, Chore "windows") insertChore
      let flexdays = FlexDaysS $ FlexDays 2
      let strict = StrictDaysS $ StrictDays 2
      let weekly = WeeklyPatternS $ Pattern (Set.fromList [(1, Mon), (2, Tue)]) 2
      let monthly = MonthlyPatternS $ Pattern (Set.fromList [(1, DayOfMonth 10), (2, DayOfMonth 20)]) 2
      scheduleId1 <- insertSchedule (choreId1, flexdays)
      scheduleId2 <- insertSchedule (choreId2, strict)
      scheduleId3 <- insertSchedule (choreId3, weekly)
      scheduleId4 <- insertSchedule (choreId4, monthly)
      output1 <- Session.statement scheduleId1 getSchedule
      output2 <- Session.statement scheduleId2 getSchedule
      output3 <- Session.statement scheduleId3 getSchedule
      output4 <- Session.statement scheduleId4 getSchedule
      liftIO $ flexdays @?= output1
      liftIO $ strict @?= output2
      liftIO $ weekly @?= output3
      liftIO $ monthly @?= output4
  ]

-- patternTests :: TestTree
-- patternTests = testGroup "Pattern Tests"
--   [  testCase "" ]
