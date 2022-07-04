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


import Models
import DB.User
import DB.Password
import DB.RefreshToken

-------------------------------------------------------------------------------
-- Test utils

-- session c s =
  -- Session.run s c >>=
  -- either (fail . show) return

withPool' :: Pool.Pool -> Session.Session a -> IO a
withPool' p s = Pool.use p s >>= either (fail . show) return

withPool :: Pool.Pool -> Session.Session a -> IO (Either Pool.UsageError a)
withPool p s = Pool.use

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
  pool <- Pool.acquire (10, 60, connStr)
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
tests runS = testGroup "Tests" [unitTests runS]

assertCompletes :: Assertion
assertCompletes  = True @?= True

unitTests :: SessionRunner -> TestTree
unitTests runS = testGroup "Query Tests"
  [ testCase "Do the thing" $ False @?= False
  -- User
  , testCase "Insert user" $ do
      runS $ Session.statement (User "test" "test") insertUser
      assertCompletes
  , testCase "Select user" $ do
      runS $ do
        newId <- Session.statement (User "test" "test") insertUser
        Session.statement newId selectUser
      --("at" :: String) @?= "no"
      assertCompletes
  -- Password
  , testCase "Password round trip id" $ do
      let pw = PasswordHash "test_password"
      dbPw <- runS $ do
        newId <- Session.statement (User "test" "test") insertUser
        Session.statement (newId, pw) insertPassword
        Session.statement newId selectPassword
      pw @?= dbPw
  , testCase "Password round trip username" $ do
      let pw = PasswordHash "test_password"
      result <- runS $ do
        newId <- Session.statement (User "test" "test") insertUser
        Session.statement (newId, pw) insertPassword
        Session.statement (Username "test") passwordInfoByUsername
      isJust result @?= True
  -- RefreshToken
  , testCase "Token round trip" $ do
      expiry <- addUTCTime 3000 <$> liftIO getCurrentTime
      now <- liftIO getCurrentTime
      let token = "test_token"
      (originalId, dbUserId) <- runS $ do
        originalId <- Session.statement (User "test" "test") insertUser
        Session.statement (originalId, token, expiry) upsertToken
        Session.statement (originalId, token, expiry) upsertToken --update
        dbUserId <- Session.statement (token, now) selectToken
        Session.statement originalId deleteToken
        return (originalId, dbUserId)
      originalId @?= fromJust dbUserId
  ]

patternTests :: TestTree
patternTests = testGroup "Pattern Tests"
  [  testCase "" ]
