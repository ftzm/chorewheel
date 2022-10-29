module ChoreWheel where

import Control.Monad.Catch qualified as C
import Control.Monad.Error.Class
import Control.Monad.Trans.Resource
import Data.UUID.V4
import Effect.Auth.Password
import Effect.Auth.Session
import Hasql.Pool qualified as HP
import Hasql.Session qualified as HS
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.Server
import Servant.Server.Generic
import Server.Root

import App
import DB
import DB.User
import Log
import Models

--------------------------------------------------------------------------------
-- Env

createAppEnv :: ResourceT IO AppEnv
createAppEnv = do
  pool <- dbResource
  (logEnv, logContext, logNamespace) <- logResource "ChoreWheel" "production"
  return $ AppEnv pool logEnv logContext logNamespace

--------------------------------------------------------------------------------
-- Application

-- TODO: Investigate how to use this to convert internal errors to servant
-- errors
appToHandler :: AppEnv -> App a -> Handler a
appToHandler env (App m) = do
  val <-
    C.catch
      (liftIO $ runExceptT $ runReaderT m env)
      -- Exception that pass this point are handled by WAI's default exception
      -- handler configured in its settings.
      -- (\(e :: SomeException) -> putStrLn $ show >> throwM e)
      (\(e :: SomeException) -> throwM e)
  case val of
    Left e -> throwError e
    Right s -> return s

choreWheelApp :: (forall a. App a -> Handler a) -> Application
choreWheelApp f = genericServeTWithContext f choreWheelApi ctx
 where
  ctx = authHandler f :. EmptyContext

createApp :: AppEnv -> Application
createApp env = requestLogMiddleWare $ choreWheelApp $ appToHandler env
 where
  requestLogMiddleWare = logRequests env.logEnv env.logNamespace

--------------------------------------------------------------------------------
-- Warp Settings

createWarpSettings :: AppEnv -> Settings
createWarpSettings env =
  defaultSettings
    & setPort 8080
    & setOnException warpLog
 where
  warpLog :: Maybe Request -> SomeException -> IO ()
  warpLog _ e =
    when (defaultShouldDisplayException e) $
      logIO env.logEnv env.logNamespace $
        show e

--------------------------------------------------------------------------------
-- Main

createTestUser :: HP.Pool -> IO ()
createTestUser p = do
  userId <- UserId <$> nextRandom
  HP.use p (HS.statement (User userId "matt" "m@test.com") insertUser)
    >>= either (fail . show) return
  HP.use p $ createPassword userId (Password "test")
  return ()

runApp :: IO ()
runApp = runResourceT $ do
  env <- createAppEnv
  let app = createApp env
      warpSettings = createWarpSettings env
  liftIO $ do
    logInfoIO env.logEnv env.logNamespace "Chorewheel started"
    runSettings warpSettings app
