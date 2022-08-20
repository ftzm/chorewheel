{-# LANGUAGE UndecidableInstances #-}

module Effect.Auth.Jwt where

import Servant.Auth.Server (JWTSettings, makeJWT)
import Data.Time.Clock
import qualified Data.ByteString.Lazy as BL
import qualified Hasql.Session as HS
import Crypto.Random.Types (getRandomBytes)
import Data.ByteString.Base64 (encode)
import Data.Traversable
import Data.Generics.Product.Typed

import Models
import DB
import DB.RefreshToken
import Effect.Auth.Password

-------------------------------------------------------------------------------
-- JWT

createJwt :: MonadIO m => JWTSettings -> UserId -> m Jwt
createJwt jwtCfg (UserId i) = do
  expiration <- addUTCTime 300 <$> liftIO getCurrentTime
  jwte <- liftIO $ makeJWT (JwtPayload i) jwtCfg $ Just expiration
  either (error . show) (pure . Jwt . BL.toStrict) jwte

-------------------------------------------------------------------------------
-- Refresh Token

-- TODO: wrap in newtype
genToken :: IO RefreshToken
genToken = RefreshToken . encode <$> getRandomBytes 64

updateToken :: UserId -> HS.Session RefreshToken
updateToken i = do
  t <- liftIO genToken
  expiry <- addUTCTime 300 <$> liftIO getCurrentTime
  HS.statement (i, decodeUtf8 $ unRefreshToken t, expiry) upsertToken
  pure t

redeemRefreshTokenImpl
  :: WithDb r m
  => ByteString
  -> m (Maybe (RefreshToken, UserId))
redeemRefreshTokenImpl token = do
  now <- liftIO getCurrentTime
  runPool $ do
    i <- HS.statement (decodeUtf8 token, now) selectToken
    t <- traverse updateToken i
    pure $ (,) <$> t <*> i

-------------------------------------------------------------------------------
-- Implementations

type WithJwtCfg r m = (MonadReader r m, HasType JWTSettings r, MonadIO m)

refreshTokensImpl ::
  WithDb r m =>
  WithJwtCfg r m =>
  RefreshToken ->
  m (Maybe (RefreshToken, Jwt))
refreshTokensImpl (RefreshToken token) = runMaybeT $ do
  jwtCfg <- asks $ getTyped @JWTSettings
  (newToken, i) <- MaybeT $ redeemRefreshTokenImpl token
  jwt <- createJwt jwtCfg i
  pure (newToken, jwt)

loginForTokensImpl ::
  WithDb r m =>
  WithJwtCfg r m =>
  Username ->
  Password ->
  m (Maybe (RefreshToken, Jwt))
loginForTokensImpl u p = do
  jwtCfg <- asks $ getTyped @JWTSettings
  runPool $ do
    userIdO <- validateBasicAuth u p
    for userIdO $ \i -> do
      (,) <$> updateToken i <*> createJwt jwtCfg i

-------------------------------------------------------------------------------
-- Interface

class Monad m => AuthM m where
  loginForTokens :: Username -> Password -> m (Maybe (RefreshToken, Jwt))
  refreshTokens :: RefreshToken -> m (Maybe (RefreshToken, Jwt))

-------------------------------------------------------------------------------
-- Instances

newtype AuthT m a = AuthT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance (WithDb r m, WithJwtCfg r m) => AuthM (AuthT m) where
  loginForTokens = loginForTokensImpl
  refreshTokens = refreshTokensImpl

newtype AuthTestT m a = AuthTestT { unAuthTest :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance (Monad m, Applicative m) => AuthM (AuthTestT m) where
  loginForTokens _ _ = pure $ Just (RefreshToken "", Jwt "")
  refreshTokens _ = pure $ Just (RefreshToken "", Jwt "")
