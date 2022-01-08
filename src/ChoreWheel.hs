{-# LANGUAGE FlexibleContexts #-}

module ChoreWheel where

import Network.Wai.Handler.Warp (run)
import Servant.Server
import Servant.Auth.Server
import Control.Monad.Reader
import           Database.Persist.Postgresql          (ConnectionPool)
import           Control.Monad.Error.Class
import Control.Monad.Except


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
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
  --run 8080 $ abstractApp jwtCfg (\m -> runExceptT $ runConsoleLogT $ runPUT pool $ m)
  run 8080 $ abstractApp jwtCfg $ appToHandler pool
