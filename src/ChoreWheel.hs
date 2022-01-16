{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ChoreWheel where

import Network.Wai.Handler.Warp (run)
import Servant.Server
import Servant.Auth.Server
import Control.Monad.Reader
import           Control.Monad.Error.Class
import           Database.Persist.Postgresql
import Control.Exception (SomeException(..))
import Control.Monad.Catch (catch)
import Data.Maybe
import qualified Data.ByteString as BS
import Crypto.KDF.BCrypt (hashPassword)
import Data.Text.Encoding
import qualified Hasql.Session as Session
import System.Exit (die)
import qualified Hasql.Pool as Pool


import DB
import API
import Models
import Log
import DB.User

theFunc :: ReaderT ConnectionPool IO a -> Handler a
theFunc = undefined

program :: (GetUsers m, MonadLog m) => m ()
program = undefined

runProgram :: IO ()
runProgram = do
  pool <- makePool
  (runPUT pool) $ runConsoleLogT $ program

toHandler' :: MonadError ServerError m => MonadIO m => UserStore m => MonadLog m => m a -> Handler a
toHandler' = undefined

createTestUser :: ConnectionPool -> IO ()
createTestUser pool = do
  -- NOTE: this is how to catch exceptions from persistent
  hashedPassword :: BS.ByteString <- liftIO $ hashPassword 10 ("test" :: BS.ByteString)
  let dbAction = do
        userKey <- insert $ DbUser "matt" "m@test.com"
        insert $ DbUserPassword (decodeUtf8 hashedPassword) userKey
        return $ Just userKey

  let mkUser =(dbAction `catch` (\(SomeException e) -> liftIO $ putStrLn ("user already exists: " ++ (show e)) >> return Nothing))
  k <- do
    k' <- liftIO $ runSqlPool mkUser pool
    case k' of
      Nothing -> (liftIO $ entityKey . fromJust <$> runSqlPool (getBy $ UniqueName "matt") pool) :: IO (Key DbUser)
      Just k'' -> (return k'') :: IO (Key DbUser)
  t <- createRefreshTokenImpl' pool k
  print t

createPool :: IO Pool.Pool
createPool = do
  config  <- parsePostgresConfig
  connStr <- case config of
    Left e -> die $ "Fuck: Error creating Postgresql Connection Pool: " <> e
    Right c -> return $ makeConnStr c
  pool <- Pool.acquire (10, 60, connStr)
  return pool

runApp :: IO ()
runApp = do
  --createdIdE <- Session.run (Session.statement (User "matt" "m@test.com") insertUser) conn
  --_ <- case createdIdE of
    --Left _ -> die "user already exists"
    --Right x -> liftIO $ print x
  pool <- createPool
  gotted <- Pool.use pool (Session.statement (UserId 1) selectUser)
  print gotted

  myKey <- generateKey
  pool' <- makePool
  createTestUser pool'
  let jwtCfg = defaultJWTSettings myKey
  run 8080 $ abstractApp jwtCfg $ appToHandler pool'
