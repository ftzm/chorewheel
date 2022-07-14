{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module DB where

import Env
import Control.Monad.Reader
import qualified Hasql.Pool as HP
import qualified Hasql.Session as HS
import Data.Generics.Product.Typed
import qualified Data.ByteString as BS
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
  BSU.fromString $ unwords
  [ "host=localhost"
  , "port=5432"
  , "dbname=postgres"
  , "user=" <> user
  , "password=" <> password
  ]

createPool :: IO HP.Pool
createPool = parsePostgresConfig >>= \case
  Left e -> die $ "Error creating Postgresql Connection Pool: " <> e
  Right c -> HP.acquire 10 $ makeConnStr c

-- TODO: Handle db errors better
runPool :: WithDb r m => HS.Session a -> m a
runPool s = do
  pool <- asks $ getTyped @HP.Pool
  liftIO $ HP.use pool s >>= either (fail . show) return
