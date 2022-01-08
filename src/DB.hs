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


module DB where

import Env
import Control.Monad.Reader
import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool, fromSqlKey)
import           Database.Persist.Postgresql          (selectList, insert, entityVal)
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

import Models

-------------------------------------------------------------------------------
-- Create DB tables

share
    [ mkPersist sqlSettings
    , mkMigrate "migrateAll"
    ] [persistLowerCase|
DbUser sql=users
    name Text
    email Text
    deriving Show Eq
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

-- write polymorphic implementation
getUsersImpl :: WithGetUsers r m => m [User]
getUsersImpl = do
    pool <- asks $ getTyped @ConnectionPool
    users <- liftIO $ runSqlPool (selectList [] []) pool
    return $ map (toUser . entityVal) users

-- create a newtype to "carry" a typeclass instance, which can later be used to
-- provide instances to other Monad stacks via "derivingVia"
newtype GetUsersT m a = GetUsersT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

-- write an instance on the above carrier type, which is just the Impl function
-- Might be better to write the implementation directly in here
instance WithGetUsers r m => MonadGetUsers (GetUsersT m) where
  getUsers' = getUsersImpl

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
