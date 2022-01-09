{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ChoreWheel where

import Network.Wai.Handler.Warp (run)
import Servant.Server
import Servant.Auth.Server
import Control.Monad.Reader
import           Database.Persist.Postgresql          (ConnectionPool)
import           Control.Monad.Error.Class
import Control.Monad.Except
import           Database.Persist.Postgresql
import Control.Exception (SomeException(..))
import Control.Monad.Catch (catch)
import Data.Maybe
import qualified Data.ByteString as BS
import Crypto.KDF.BCrypt (hashPassword)
import Data.Text.Encoding


import DB
import API
import Models
import Log

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

appToHandler :: ConnectionPool -> App a -> Handler a
appToHandler pool (App m) = do
  val <- liftIO $ runExceptT $ runReaderT m $ AppEnv pool
  case val of
    Left e -> throwError e
    Right s -> return s



runApp :: IO ()
runApp = do
  pool <- makePool
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
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
  --run 8080 $ abstractApp jwtCfg (\m -> runExceptT $ runConsoleLogT $ runPUT pool $ m)
  run 8080 $ abstractApp jwtCfg $ appToHandler pool
