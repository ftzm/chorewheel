module DB where

import Control.Monad.Trans.Resource
import Data.ByteString qualified as BS
import Data.Generics.Product.Typed
import Data.Text (pack)
import Env
import Hasql.Pool qualified as HP
import Hasql.Session qualified as HS

type WithDb r m = (MonadReader r m, HasType HP.Pool r, MonadIO m)

data PostgresConfig = PostgresConfig
  { user :: Text
  , password :: Text
  }
  deriving (Show, Eq)

text :: Env.Reader e Text
text = Right . pack

parsePostgresConfig :: IO (Either String PostgresConfig)
parsePostgresConfig =
  Env.parseOr pure (header "Postgresql Environment") $
    PostgresConfig
      <$> var (text <=< nonempty) "POSTGRES_USER" (help "User" <> def "user")
      <*> var (text <=< nonempty) "POSTGRES_PASSWORD" (help "Password" <> def "hunter2")

makeConnStr :: PostgresConfig -> BS.ByteString
makeConnStr PostgresConfig{..} =
  encodeUtf8 $
    unwords
      [ "host=localhost"
      , "port=5432"
      , "dbname=postgres"
      , "user=" <> user
      , "password=" <> password
      ]

createPool :: IO HP.Pool
createPool =
  parsePostgresConfig >>= \case
    Left e -> die $ "Error creating Postgresql Connection Pool: " <> e
    Right c -> HP.acquire 10 Nothing $ makeConnStr c

-- TODO: Handle db errors better
runPool :: WithDb r m => HS.Session a -> m a
runPool s = do
  pool <- asks $ getTyped @HP.Pool
  liftIO $ HP.use pool s >>= either throwM return

dbResource :: MonadResource m => m (HP.Pool)
dbResource = snd <$> allocate createPool HP.release
