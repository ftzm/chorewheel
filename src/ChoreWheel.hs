{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module ChoreWheel where

import Network.Wai.Handler.Warp (run)
import Servant.Server
import Servant.Server.Generic
import Servant.Auth.Server
import Control.Monad.Reader
import           Control.Monad.Error.Class
import Control.Monad.Except
  --
-- import qualified Hasql.Pool as HP
-- import Effect.Auth
-- import Models

import App
import API.Root
import DB

-- TODO: Investigate how to use this to convert internal errors to servant errors
appToHandler :: AppEnv -> App a -> Handler a
appToHandler env (App m) = do
  val <- liftIO $ runExceptT $ runReaderT m env
  case val of
    Left e -> throwError e
    Right s -> return s

choreWheelApp :: JWTSettings -> (forall a. App a -> Handler a) -> Application
choreWheelApp jwtCfg f = genericServeTWithContext f choreWheelApi ctx
  where
    ctx = defaultCookieSettings :. jwtCfg :. EmptyContext

runApp :: IO ()
runApp = do
  pool <- createPool
  --HP.use pool $ createPassword (UserId 1) (Password"test")
  jwtCfg <- defaultJWTSettings <$> generateKey
  let env = AppEnv { _pool = pool
                   , _jwtCfg = jwtCfg
                   }
  run 8080 $ choreWheelApp jwtCfg $ appToHandler env
