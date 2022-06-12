{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module DB where

import Env
import Control.Monad.Reader
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import Data.Generics.Product.Typed
import qualified Data.ByteString as BS
import Data.List (intercalate)
import qualified Data.ByteString.UTF8 as BSU
import System.Exit (die)

type WithDb r m = (MonadReader r m, HasType HP.Pool r, MonadIO m)

data PostgresConfig = PostgresConfig
  { user :: String
  , password :: String
  } deriving (Show, Eq)

parsePostgresConfig :: IO (Either String PostgresConfig)
parsePostgresConfig =
  Env.parseOr pure (header "Postgresql Environment") $ PostgresConfig
  <$> var (str <=< nonempty) "POSTGRES_USER"      (help "User" <> def "user")
  <*> var (str <=< nonempty) "POSTGRES_PASSWORD"  (help "Password" <> def "hunter2")

makeConnStr :: PostgresConfig -> BS.ByteString
makeConnStr PostgresConfig {..} =
  BSU.fromString $ intercalate " "
  [ "host=localhost"
  , "port=5432"
  , "dbname=postgres"
  , "user=" <> user
  , "password=" <> password
  ]

createPool :: IO HP.Pool
createPool = do
  config  <- parsePostgresConfig
  connStr <- case config of
    Left e -> die $ "Fuck: Error creating Postgresql Connection Pool: " <> e
    Right c -> return $ makeConnStr c
  -- pool <- HP.acquire (10, 60, connStr)
  -- pool siz timeout connstr
  pool <- HP.acquire 10 connStr
  return pool

-- TODO: Handle db errors better
runPool :: WithDb r m => HS.Session a -> m a
runPool s = do
  pool <- asks $ getTyped @HP.Pool
  liftIO $ HP.use pool s >>= either (fail . show) return
