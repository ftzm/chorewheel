module App where

import Control.Monad.Except
import Hasql.Pool qualified as HP
import Katip (LogContexts, LogEnv, Namespace)
import Servant.Server (ServerError)

import Effect.Auth.Session
import Effect.Chore
import Effect.Household
import Effect.Identifier
import Effect.Time
import Effect.User

data AppEnv = AppEnv
  { pool :: HP.Pool
  , logEnv :: LogEnv
  , logContexts :: LogContexts
  , logNamespace :: Namespace
  }
  deriving (Generic)

newtype App a = App (ReaderT AppEnv (ExceptT ServerError IO) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppEnv
    , MonadError ServerError
    )
  deriving (TimeM) via (TimeT App)
  deriving (IdentifierM) via (IdentifierT App)
  deriving (SessionAuthM) via (SessionAuthT App)
  deriving (UserM) via (UserT App)
  deriving (HouseholdM) via (HouseholdT App)
  deriving (ChoreM) via (ChoreT App)
