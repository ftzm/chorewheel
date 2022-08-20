module ChoreWheel where

import Network.Wai.Handler.Warp (run)
import Servant.Server
import Servant.Server.Generic
import Servant.Auth.Server
import           Control.Monad.Error.Class
--
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import Effect.Auth.Session
import Effect.Auth.Password
import DB.User
import Models
import Data.UUID.V4

import App
import Server.Root
import DB

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
  val <- liftIO $ runExceptT $ runReaderT m env
  case val of
    Left e -> throwError e
    Right s -> return s

choreWheelApp :: (forall a. App a -> Handler a) -> Application
choreWheelApp f = genericServeTWithContext f choreWheelApi ctx
  where
    --ctx = defaultCookieSettings :. jwtCfg :. EmptyContext
    ctx = authHandler f :. EmptyContext

runApp :: IO ()
runApp = do
  pool <- createPool
  --createTestUser pool
  jwtCfg <- defaultJWTSettings <$> generateKey
  let env = AppEnv pool jwtCfg
  run 8080 $ choreWheelApp $ appToHandler env
