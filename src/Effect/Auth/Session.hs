{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Effect.Auth.Session where

import Data.ByteString.Base64 (encode)
import Crypto.Random.Types (getRandomBytes)
import qualified Hasql.Session as HS
import Data.Time.Clock
import Web.Cookie                       (parseCookies)
import Data.List (lookup)

--servant
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                         mkAuthHandler)
import Network.Wai                      (Request, requestHeaders)
import qualified Data.ByteString.Lazy as LBS

import Models
import DB
import DB.Session
import Effect.Auth.Password

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
  where sixtyDaysInSeconds = 5184000

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

getCookieOrError :: Request -> ByteString -> Either LBS.ByteString Text
getCookieOrError req name' = do
  cookieHeader <- maybeToEither
    "Missing cookie header"
    (lookup "cookie" $ requestHeaders req)
  cookie <- maybeToEither
    (fromStrict $ "Missing cookie: " <> name')
    (lookup name' $ parseCookies cookieHeader)
  pure $ decodeUtf8 cookie
  where
    maybeToEither e = maybe (Left e) Right

authHandler :: SessionAuthM m => (forall a. m a -> Handler a) -> AuthHandler Request UserId
authHandler f = mkAuthHandler $ \r ->
  either throw401 handle (SessionToken <$> getCookieOrError r "session-token")
  where
    throw401 msg = throwError $ err401 {errBody = msg}
    handle = maybe (throw401 "Session cookie invalid") pure <=< f . continue
