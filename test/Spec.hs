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
import Data.Either
import Data.Functor
import Database.Postgres.Temp
import Data.ByteString(readFile)


import Models
import DB.User

-------------------------------------------------------------------------------
-- Test utils

-- session c s =
  -- Session.run s c >>=
  -- either (fail . show) return

withPool' :: Pool.Pool -> Session.Session a -> IO a
withPool' p s = Pool.use p s >>= either (fail . show) return

withPool :: Pool.Pool -> Session.Session a -> IO (Either Pool.UsageError a)
withPool p s = Pool.use p s

-- transaction' connection transaction =
  -- session connection (TS.transaction TS.RepeatableRead TS.Write transaction)

poolTransS :: Pool.Pool -> a -> S.Statement a b -> IO (Either Pool.UsageError b)
poolTransS pool input statement =
  withPool pool $ TS.transaction TS.RepeatableRead TS.Write $ do
                    output <- T.statement input statement
                    T.condemn
                    return output

withRollback :: Pool.Pool -> Session.Session a -> IO (Either Pool.UsageError a)
withRollback p s = Pool.use p $ Session.sql "BEGIN" *> s <* Session.sql "ROLLBACK"

createTestPool :: DB -> IO Pool.Pool
createTestPool db = do
  let connStr = toConnectionString db
  pool <- Pool.acquire (10, 60, connStr)
  migration <- readFile("migration/init.sql")
  Pool.use pool $ Session.sql migration
  return pool

type SessionRunner = forall a. Session.Session a -> IO (Either Pool.UsageError a)

-------------------------------------------------------------------------------
main :: IO ()
main = void $ with $ \db -> do
  pool <- createTestPool db
  let runS = withRollback pool
  defaultMain $ tests runS

tests :: SessionRunner -> TestTree
tests runS = testGroup "Tests" [unitTests runS]

unitTests :: SessionRunner -> TestTree
unitTests runS = testGroup "Query Tests"
  [ testCase "Do the thing" $ False @?= False
  , testCase "The other" $ return () >>= \x -> x @?= ()
  -- User
  , testCase "Insert user" $ do
      x <- runS $ Session.statement (User "test" "test") insertUser
      isRight x @?= True
  , testCase "Insert user 2" $ do
      x <- runS $ Session.statement (User "test" "test") insertUser
      isRight x @?= True
  , testCase "Select user" $ do
      result <- runS $ do
        newId <- Session.statement (User "test" "test") insertUser
        Session.statement newId selectUser
      isRight result @?= True
  -- Password
  ]
