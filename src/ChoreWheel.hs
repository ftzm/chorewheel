module ChoreWheel where

import Servant.Server
import Servant.Server.Generic
import           Control.Monad.Error.Class
--
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import Effect.Auth.Session
import Effect.Auth.Password
import DB.User
import Models
import Data.UUID.V4
import Log
import Network.Wai

import App
import Server.Root
import DB
import qualified Control.Monad.Catch as C
import Network.Wai.Handler.Warp

import Control.Monad.Trans.Resource

createTestUser :: HP.Pool -> IO ()
createTestUser p = do
  userId <- UserId <$> nextRandom
  HP.use p ( HS.statement (User userId "matt" "m@test.com") insertUser)
    >>= either (fail . show) return
  HP.use p $ createPassword userId (Password "test")
  return ()

-- TODO: Investigate how to use this to convert internal errors to servant errors
appToHandler :: AppEnv -> App a -> Handler a
appToHandler env (App m) = do
  val <- C.catch (liftIO $ runExceptT $ runReaderT m env)
    -- Exception that pass this point are handled by WAI's default exception
    -- handler configured in its settings.
    --(\(e :: SomeException) -> putStrLn $ show >> throwM e)
    (\(e :: SomeException) ->  throwM e)
  case val of
    Left e -> throwError e
    Right s -> return s

choreWheelApp :: (forall a. App a -> Handler a) -> Application
choreWheelApp f = genericServeTWithContext f choreWheelApi ctx
  where
    ctx = authHandler f :. EmptyContext

runApp :: IO ()
runApp = runResourceT $ do
  pool <- dbResource
  userId <- liftIO $ UserId <$> nextRandom
  liftIO $ HP.use pool ( HS.statement (User userId "stina" "s@test.com") insertUser)
  --_ <- liftIO $ createTestUser pool
  (logEnv, logContext, logNamespace) <- logResource "ChoreWheel" "production"
  let env = AppEnv pool logEnv logContext logNamespace
  let warpLog :: Maybe Request -> SomeException -> IO () =
        \_ e -> when (defaultShouldDisplayException e)
        $ logIO logEnv logNamespace $ show e
  let warpSettings = defaultSettings
        & setPort 8080
        & setOnException warpLog
  liftIO $ logInfoIO logEnv logNamespace "Chorewheel started"
  liftIO
    $ runSettings warpSettings
    $ logRequests logEnv logNamespace
    $ choreWheelApp
    $ appToHandler env
