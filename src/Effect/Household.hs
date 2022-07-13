{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}

module Effect.Household where

import Control.Monad.Reader
import qualified Hasql.Session as HS
import qualified Data.Vector as V
import Data.Text (Text)

import DB
import DB.Household
import Models

class Monad m => HouseholdM m where
  getHouseholds :: UserId -> m [(HouseholdId, Household)]
  createHousehold :: UserId -> Household -> m ()
  leaveHousehold :: UserId -> HouseholdId -> m ()
  householdIdFromName :: UserId -> Text -> m (Maybe HouseholdId)

newtype HouseholdT m a = HouseholdT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance (WithDb r m) => HouseholdM (HouseholdT m) where
  getHouseholds i = runPool $ HS.statement i (V.toList <$> getUserHouseholds)
  createHousehold u h = runPool $ do
    householdId <- HS.statement h insertHousehold
    HS.statement (householdId, u) insertHouseholdMember
  leaveHousehold u h = runPool $ do
    HS.statement (h, u) removeHouseholdMember
    HS.statement h deleteEmptyHousehold
  householdIdFromName u n = runPool $ HS.statement (u, n) getHouseholdIdFromName