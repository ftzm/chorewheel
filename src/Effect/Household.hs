{-# LANGUAGE UndecidableInstances #-}

module Effect.Household where

import Data.UUID.V4
import Data.Vector qualified as V
import Hasql.Session qualified as HS

import DB
import DB.Household
import Models

data HouseholdMembership = HouseholdMembership
  { userId :: UserId
  , householdId :: HouseholdId
  }

class Monad m => HouseholdM m where
  getHouseholds :: UserId -> m [Household]
  getHousehold :: UserId -> HouseholdId -> m (Maybe Household)
  createHousehold :: UserId -> Text -> m ()
  leaveHousehold :: UserId -> HouseholdId -> m ()
  householdIdFromName :: UserId -> Text -> m (Maybe HouseholdId)
  householdFromName :: UserId -> Text -> m (Maybe Household)
  getHouseholdMembership ::
    UserId ->
    HouseholdId ->
    m (Maybe HouseholdMembership)

newtype HouseholdT m a = HouseholdT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance (WithDb r m) => HouseholdM (HouseholdT m) where
  getHouseholds i = runPool $ HS.statement i (V.toList <$> getUserHouseholds)
  getHousehold i hId = runPool $ HS.statement (i, hId) getHouseholdById
  createHousehold u h = runPool $ do
    householdId <- HouseholdId <$> liftIO nextRandom
    HS.statement (householdId, h) insertHousehold
    HS.statement (householdId, u) insertHouseholdMember
  leaveHousehold u h = runPool $ do
    HS.statement (h, u) removeHouseholdMember
    HS.statement h deleteEmptyHousehold
  householdIdFromName u n = runPool $ HS.statement (u, n) getHouseholdIdFromName
  householdFromName u n = runPool $ HS.statement (u, n) getHouseholdByName
  getHouseholdMembership u h = do
    isMember <- runPool $ HS.statement (u, h) isHouseholdMemberQ
    return $ if isMember then Just (HouseholdMembership u h) else Nothing
