{-# LANGUAGE UndecidableInstances #-}

module Effect.Household where

import qualified Hasql.Session as HS
import qualified Data.Vector as V
import Data.UUID.V4

import DB
import DB.Household
import Models

class Monad m => HouseholdM m where
  getHouseholds :: UserId -> m [Household]
  createHousehold :: UserId -> Text -> m ()
  leaveHousehold :: UserId -> HouseholdId -> m ()
  householdIdFromName :: UserId -> Text -> m (Maybe HouseholdId)
  householdFromName :: UserId -> Text -> m (Maybe Household)

newtype HouseholdT m a = HouseholdT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance (WithDb r m) => HouseholdM (HouseholdT m) where
  getHouseholds i = runPool $ HS.statement i (V.toList <$> getUserHouseholds)
  createHousehold u h = runPool $ do
    householdId <- HouseholdId <$> liftIO nextRandom
    HS.statement (householdId, h) insertHousehold
    HS.statement (householdId, u) insertHouseholdMember
  leaveHousehold u h = runPool $ do
    HS.statement (h, u) removeHouseholdMember
    HS.statement h deleteEmptyHousehold
  householdIdFromName u n = runPool $ HS.statement (u, n) getHouseholdIdFromName
  householdFromName u n = runPool $ HS.statement (u, n) getHouseholdByName
