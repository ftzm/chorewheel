module App where

import Control.Monad.Except
import qualified Hasql.Pool as HP
import Servant.Server (ServerError)
import Katip (LogEnv, LogContexts, Namespace)

import Effect.Auth.Session
import Effect.User
import Effect.Household
import Effect.Chore

data AppEnv = AppEnv
  { _pool :: HP.Pool
  , _logEnv :: LogEnv
  , _logContexts :: LogContexts
  , _namespace :: Namespace
  } deriving Generic

newtype App a = App (ReaderT AppEnv (ExceptT ServerError IO) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppEnv
    , MonadError ServerError
    )
  deriving SessionAuthM via (SessionAuthT App)
  deriving UserM via (UserT App)
  deriving HouseholdM via (HouseholdT App)
  deriving ChoreM via (ChoreT App)
