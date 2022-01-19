{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Effect.Auth where

import Servant.Auth.Server (JWTSettings, makeJWT)
import Control.Monad.IO.Class
import Data.Time.Clock
import qualified Data.ByteString.Lazy as BL
import Data.ByteString
import qualified Hasql.Session as HS
import Data.Text.Encoding
import Control.Monad.Trans.Maybe
import Crypto.Random.Types (getRandomBytes)
import Data.ByteString.Base64 (encode)
import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import Data.Bool
import Data.Generics.Product.Typed
import Control.Monad.Reader

import Models
import DB
import DB.RefreshToken
import DB.Password

-------------------------------------------------------------------------------
-- Password

validateBasicAuth :: Username -> Password -> HS.Session (Maybe UserId)
validateBasicAuth u p = do
  result <-  HS.statement u passwordInfoByUsername
  pure $ result >>= \(i, dbPasswordHash) ->
    bool Nothing (Just i) $ validatePassword
     (unPassword p) (encodeUtf8 $ unPasswordHash dbPasswordHash)

createPassword :: UserId -> Password -> HS.Session ()
createPassword u (Password p) = do
  h <- liftIO $ PasswordHash . decodeUtf8 <$> hashPassword 10 p
  HS.statement (u, h) insertPassword

-------------------------------------------------------------------------------
-- JWT

createJwt :: MonadIO m => JWTSettings -> UserId -> m Jwt
createJwt jwtCfg (UserId i) = do
  expiration <- addUTCTime (fromInteger 300) <$> liftIO getCurrentTime
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
  expiry <- addUTCTime (fromInteger 300) <$> liftIO getCurrentTime
  HS.statement (i, (decodeUtf8 $ unRefreshToken t), expiry) upsertToken
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
    flip traverse userIdO $ \i -> do
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

newtype AuthTestT m a = AuthTestT { unAuthTest :: (m a) }
  deriving newtype (Functor, Applicative, Monad)

instance (Monad m, Applicative m) => AuthM (AuthTestT m) where
  loginForTokens _ _ = pure $ Just (RefreshToken "", Jwt "")
  refreshTokens _ = pure $ Just (RefreshToken "", Jwt "")
