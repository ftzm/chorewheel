{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Effect.Auth.Session where

import Control.Monad.Except
import Crypto.Random.Types (getRandomBytes)
import Data.ByteString.Base64 (encode)
import Data.List (lookup)
import Data.Time.Clock
import Hasql.Session qualified as HS
import Network.Wai (Request, requestHeaders)
import Servant
import Servant.Server.Experimental.Auth (
  AuthHandler,
  AuthServerData,
  mkAuthHandler,
 )
import Web.Cookie (parseCookies)

import ApiUtil
import DB
import DB.Session
import Effect.Auth.Password
import Log
import Models
import Routes.Root

-------------------------------------------------------------------------------
-- Implementation

genToken :: IO Text
genToken = decodeUtf8 . encode <$> getRandomBytes 64

createSessionImpl :: UserId -> HS.Session SessionToken
createSessionImpl u = do
  t <- SessionToken <$> liftIO genToken
  expiration <- addUTCTime sixtyDaysInSeconds <$> liftIO getCurrentTime
  HS.statement (u, t, expiration) upsertToken
  return t
 where
  sixtyDaysInSeconds = 5184000

loginImpl :: WithDb r m => Username -> Password -> m (Maybe SessionToken)
loginImpl u p = runPool $ traverse createSessionImpl =<< validateBasicAuth u p

continueSessionImpl :: WithDb r m => SessionToken -> m (Maybe UserId)
continueSessionImpl token = do
  expiration <- addUTCTime 5184000 <$> liftIO getCurrentTime
  runPool $ HS.statement (token, expiration) extendToken

killSessionImpl :: WithDb r m => UserId -> m ()
killSessionImpl u = runPool $ HS.statement u deleteToken

-------------------------------------------------------------------------------
-- Interface

class Monad m => SessionAuthM m where
  login :: Username -> Password -> m (Maybe SessionToken)
  continue :: SessionToken -> m (Maybe UserId)
  logout :: UserId -> m ()

-------------------------------------------------------------------------------
-- Instances

newtype SessionAuthT m a = SessionAuthT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance WithDb r m => SessionAuthM (SessionAuthT m) where
  login = loginImpl
  continue = continueSessionImpl
  logout = killSessionImpl

-------------------------------------------------------------------------------
-- Servant Auth Handler

type instance AuthServerData (AuthProtect "session-auth") = UserId

getCookieOrError :: Request -> ByteString -> Either Text Text
getCookieOrError req name' = do
  cookieHeader <-
    maybeToEither
      "Missing cookie header"
      (lookup "cookie" $ requestHeaders req)
  cookie <-
    maybeToEither
      ("Missing cookie: " <> decodeUtf8 name')
      (lookup name' $ parseCookies cookieHeader)
  pure $ decodeUtf8 cookie
 where
  maybeToEither e = maybe (Left e) Right

authHandler' ::
  MonadError ServerError m =>
  SessionAuthM m =>
  LogM m =>
  Request ->
  m UserId
authHandler' req =
  getCookieOrError req "session-token" & either handleError checkToken
 where
  checkToken t =
    continue (SessionToken t)
      >>= maybe (handleError "session token invalid") return
  handleError msg = logWarn msg >> redirect302 rootLinks._login

authHandler ::
  MonadError ServerError m =>
  SessionAuthM m =>
  LogM m =>
  (forall a. m a -> Handler a) ->
  AuthHandler Request UserId
authHandler f = mkAuthHandler $ f . authHandler'
