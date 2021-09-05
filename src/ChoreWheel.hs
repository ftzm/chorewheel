module ChoreWheel where

import Network.Wai.Handler.Warp (run)
import Servant.Server
import Control.Monad.Reader
import           Database.Persist.Postgresql          (ConnectionPool)

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
  case pool of
    Nothing -> do
      putStrLn "No connection"
      return ()
    Just p -> (runPUT p) $ runConsoleLogT $ program

runApp :: IO ()
runApp = do
  pool <- makePool
  case pool of
    Nothing -> do
      putStrLn "No connection"
      return ()
    Just p -> do
      --mkUsers p
      run 8080 $ abstractApp (\m -> runConsoleLogT $ runPUT p $ m)
