{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module DB where

import Env
import Control.Monad.Reader
import           Database.Persist.Postgresql
import qualified Data.ByteString.Char8                as BS
import Data.List (intercalate)
import Data.Text (Text, pack)
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import           Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Control.Monad.Logger (runStdoutLoggingT)
import System.Exit (die)
import GHC.Generics
import Data.Generics.Product.Typed
import Data.Time.Clock
import Data.Text.Encoding
import Data.ByteString.Base64 (decodeLenient)
import Data.Bifunctor
import Crypto.KDF.BCrypt (validatePassword)


import Models
import Auth

-------------------------------------------------------------------------------
-- Create DB tables

share
    [ mkPersist sqlSettings
    , mkMigrate "migrateAll"
    ] [persistLowerCase|
DbUser sql=user
    name Text
    email Text
    UniqueName name
    UniqueEmail email
    deriving Show Eq

DbRefreshToken sql=refresh_token
    tokenString Text
    userId DbUserId
    expiry UTCTime
    RefreshTokenUniqueUserId userId
    UniqueTokenString tokenString
    deriving Show Eq

DbUserPassword sql=user_password
    passwordHash Text
    userId DbUserId
    UserPasswordUniqueUserId userId
|]

-------------------------------------------------------------------------------
-- Create Postgres Connection Pool

data PostgresConfig = PostgresConfig
  { user :: String
  , password :: String
  } deriving (Show, Eq)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe e = case e of
  Right x -> Just x
  Left _ -> Nothing

parsePostgresConfig :: IO (Either String PostgresConfig)
parsePostgresConfig =
          Env.parseOr pure (header "Postgresql Environment") $ PostgresConfig
          <$> var (str <=< nonempty) "POSTGRES_USER"      (help "User")
          <*> var (str <=< nonempty) "POSTGRES_PASSWORD"  (help "Password")

makeConnStr :: PostgresConfig -> ConnectionString
makeConnStr PostgresConfig {..} =
  BS.pack $ intercalate " "
  [ "host=localhost"
  , "port=5432"
  , "dbname=postgres"
  , "user=" <> user
  , "password=" <> password
  ]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

makePool :: IO ConnectionPool
makePool = do
  config  <- parsePostgresConfig
  case config of
    Left e -> die $ "Error creating Postgresql Connection Pool: " <> e
    Right c  -> do
      pool <- (runStdoutLoggingT $ createPostgresqlPool (makeConnStr c) 1)
      runSqlPool doMigrations pool
      return pool


-------------------------------------------------------------------------------
-- Refresh Token

type WithDb r m = (MonadReader r m, HasType ConnectionPool r, MonadIO m)

-- TODO: refactor to use plain IO impls and get pool in instance def

-- write polymorphic implementation
-- TODO: put everything in one runSqlPool to put it in one transaction
-- TODO: check expiry
redeemRefreshTokenImpl :: WithDb r m => BS.ByteString -> m (Maybe (BS.ByteString, Key DbUser))
redeemRefreshTokenImpl token = do
    pool <- asks $ getTyped @ConnectionPool
    dbToken <- liftIO $ runSqlPool (getBy $ UniqueTokenString $ decodeUtf8 token) pool
    case dbToken of
      Nothing -> return Nothing
      Just Entity{..} -> do
        _ <- liftIO $ runSqlPool (delete $ entityKey) pool
        newToken <- liftIO genToken
        newExpiry <- addUTCTime (fromInteger 3000) <$> liftIO getCurrentTime
        let userId = dbRefreshTokenUserId entityVal
        _ <- liftIO $ runSqlPool (insert $ DbRefreshToken (decodeUtf8 newToken) userId newExpiry) pool
        return $ Just (newToken, userId)

-- write polymorphic implementation
createRefreshTokenImpl :: WithDb r m => Key DbUser -> m BS.ByteString
createRefreshTokenImpl userId = do
    pool <- asks $ getTyped @ConnectionPool
    _ <- liftIO $ runSqlPool (deleteBy $ RefreshTokenUniqueUserId userId) pool
    newToken <- liftIO genToken
    newExpiry <- addUTCTime (fromInteger 3000) <$> liftIO getCurrentTime
    _ <- liftIO $ runSqlPool (insert $ DbRefreshToken (decodeUtf8 newToken) userId newExpiry) pool
    return newToken

-- write polymorphic implementation
createRefreshTokenImpl' :: ConnectionPool -> Key DbUser -> IO BS.ByteString
createRefreshTokenImpl' pool userId = do
    _ <- liftIO $ runSqlPool (deleteBy $ RefreshTokenUniqueUserId userId) pool
    newToken <- liftIO genToken
    newExpiry <- addUTCTime (fromInteger 3000) <$> liftIO getCurrentTime
    _ <- liftIO $ runSqlPool (insert $ DbRefreshToken (decodeUtf8 newToken) userId newExpiry) pool
    return newToken

-- TODO: Don't use db type for user id

-- define interface typeclass
class Monad m => RefreshToken m where
  redeemRefreshToken :: BS.ByteString -> m (Maybe (BS.ByteString, Key DbUser))
  createRefreshToken :: Key DbUser -> m BS.ByteString

-- create a newtype to "carry" a typeclass instance, which can later be used to
-- provide instances to other Monad stacks via "derivingVia"
newtype RefreshTokenT m a = RefreshTokenT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

-- write an instance on the above carrier type (which is just the Impl func)
-- Might be better to write the implementation directly in here
instance WithDb r m => RefreshToken (RefreshTokenT m) where
  redeemRefreshToken = redeemRefreshTokenImpl
  createRefreshToken = createRefreshTokenImpl

-------------------------------------------------------------------------------
-- getPasswordHash

getPasswordHashImpl :: WithDb r m => Text -> m (Maybe ((Key DbUser), Text))
getPasswordHashImpl name = do
    pool <- asks $ getTyped @ConnectionPool
    userO <- liftIO $ runSqlPool (getBy $ UniqueName name) pool
    password <- case userO of
      Nothing -> return Nothing
      Just user -> do
        password <- liftIO $ runSqlPool (getBy $ UserPasswordUniqueUserId $ entityKey user) pool
        let pass = dbUserPasswordPasswordHash . entityVal <$> password
        return $ (entityKey user, ) <$> pass
    return password

validateBasicAuth :: WithDb r m  => Text -> m (Maybe (Key DbUser))
validateBasicAuth encodedInput = do
  let asBs :: BS.ByteString = encodeUtf8 encodedInput
  let decoded :: BS.ByteString = decodeLenient asBs
  let (user', password') = second (BS.drop 1) . BS.break (== ':') $ decoded
  liftIO $ print (user', password')
  passwordO <- getPasswordHashImpl $ decodeUtf8 user'
  case passwordO of
    Nothing -> liftIO $ putStrLn "No password" *> return Nothing
    Just (userId, password) -> case validatePassword password' $ encodeUtf8 password of
      False -> liftIO $ putStrLn "Invalid password" *> return Nothing
      True -> do
        return $ Just $ userId



-------------------------------------------------------------------------------
-- Persistent-based Typeclass Instances

toUser :: DbUser -> User
toUser DbUser {dbUserName, dbUserEmail} = User dbUserName dbUserEmail

mkUsers :: ConnectionPool -> IO ()
mkUsers p = do
      putStrLn "name"
      name <- getLine
      putStrLn "email"
      email <- getLine
      runSqlPool (insert $ DbUser (pack name) (pack email)) p
      return ()

-- Transformer for Persistent Storage Backend
newtype PersistentDbT m a
  = PersistentDbT { unPersistentDbT :: ReaderT ConnectionPool m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance MonadIO m => GetUsers (PersistentDbT m) where
  getUsers = PersistentDbT do
    pool <- ask
    users <- liftIO $ runSqlPool (selectList [] []) pool
    return $ map (toUser . entityVal) users

instance MonadIO m => SetUser (PersistentDbT m) where
  setUser (User { name, email}) = PersistentDbT do
    pool <- ask
    key  <- liftIO $ runSqlPool (insert $ DbUser name email) pool
    return $ show $ fromSqlKey key

runPUT :: MonadIO m => ConnectionPool -> PersistentDbT m a -> m a
runPUT pool (PersistentDbT m) = runReaderT m pool

--------------------------------------------------------------------------------
-- Testing new shit

type WithGetUsers r m = (MonadReader r m, HasType ConnectionPool r, MonadIO m)

-- define interface typeclass
class Monad m => MonadGetUsers m where
  getUsers' :: m [User]
  getUser :: Int -> m (Maybe User)

-- write polymorphic implementation
getUsersImpl :: WithGetUsers r m => m [User]
getUsersImpl = do
    pool <- asks $ getTyped @ConnectionPool
    users <- liftIO $ runSqlPool (selectList [] []) pool
    return $ map (toUser . entityVal) users

-- write polymorphic implementation
getUserImpl :: WithGetUsers r m => Int -> m (Maybe User)
getUserImpl userId = do
    pool <- asks $ getTyped @ConnectionPool
    let userId' :: Key DbUser = toSqlKey $ fromIntegral userId
    user <- liftIO $ runSqlPool (get $ userId') pool
    return $ toUser <$> user

-- create a newtype to "carry" a typeclass instance, which can later be used to
-- provide instances to other Monad stacks via "derivingVia"
newtype GetUsersT m a = GetUsersT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

-- write an instance on the above carrier type, which is just the Impl function
-- Might be better to write the implementation directly in here
instance WithGetUsers r m => MonadGetUsers (GetUsersT m) where
  getUsers' = getUsersImpl
  getUser = getUserImpl

-- Append holding connection pool
-- We derive generic so that we can use `HasType` on it
data AppEnv = AppEnv {
  conectionPool :: ConnectionPool
  } deriving Generic

-- Create a concrete App monad stack and derive MonadGetUsers instance for it
-- using "GetUsersT"
newtype App' a = App' (ReaderT AppEnv IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)
  deriving MonadGetUsers via (GetUsersT App')

testApp :: App' [User]
testApp = do
  getUsers'

polyBoye :: MonadGetUsers m => m [User]
polyBoye = getUsers'

testApp' :: App' [User]
testApp' = polyBoye
