{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Models where

import Servant.Auth.Server
import Servant.API
import qualified Data.Set.NonEmpty as NESet

--import Data.Generics.Internal.VL.Lens
import Data.Generics.Labels()

import Data.Aeson
import Data.UUID

data User = User
  { id' :: UserId
  , name :: Text
  , email :: Text
  } deriving (Show, Eq, Generic, Ord)

instance ToJSON User -- generated via Generic
instance FromJSON User -- generated via Generic
instance ToJWT User
instance FromJWT User

data JwtPayload = JwtPayload
  { userId'' :: UUID
  } deriving (Show, Eq, Generic)

instance ToJSON JwtPayload -- generated via Generic
instance FromJSON JwtPayload -- generated via Generic

instance ToJWT JwtPayload
instance FromJWT JwtPayload

class Monad m => GetUsers m where
  getUsers :: m [User]

class Monad m => SetUser m where
  setUser :: User -> m String

type UserStore m =
  ( GetUsers m
  , SetUser m
  )

-- Pass-through instance for transformers
instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , GetUsers m
  ) => GetUsers (t m) where
  getUsers = lift getUsers

newtype UserId = UserId {unUserId :: UUID}
  deriving (Show, Eq, Ord, Generic)

instance ToJSON UserId -- generated via Generic
instance FromJSON UserId -- generated via Generic

newtype Username = Username { unUsername :: Text }
  deriving Show

newtype RefreshToken = RefreshToken { unRefreshToken :: ByteString }
  deriving Show

newtype Jwt = Jwt { unJwt :: ByteString }
  deriving Show

newtype Password = Password { unPassword :: ByteString }
  deriving (Show, Eq)

newtype PasswordHash = PasswordHash { unPasswordHash :: Text }
  deriving (Show, Eq)

newtype SessionToken = SessionToken { unSessionToken :: Text }
  deriving (Show, Eq)

newtype HouseholdId  = HouseholdId { unHouseholdId :: UUID }
  deriving (Show, Eq)
  deriving newtype (ToHttpApiData, FromHttpApiData)

newtype HouseholdMembers =
  HouseholdMembers { unHouseholdMembers :: NESet.NESet User }
  deriving (Show, Eq)

data Household  = Household
  { id' :: HouseholdId
  , name :: Text
  , members :: HouseholdMembers
  } deriving (Show, Eq, Generic)


data Clash = Clash { name :: Text }

-- h :: Household
-- h = Household (HouseholdId nil) "house"
--
-- y :: Text
-- y = h ^. #name
--
-- z :: Household
-- z = h & #name .~ "this"
