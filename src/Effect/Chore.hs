{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}

module Effect.Chore where

import qualified Hasql.Session as HS
import qualified Data.Vector as V

import Models
import Chore
import Schedule
import DB
import DB.Chore

class Monad m => ChoreM m where
  getFullChores :: HouseholdId -> m [(Chore, ScheduleState)]

newtype ChoreT m a = ChoreT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance (WithDb r m) => ChoreM (ChoreT m) where
  getFullChores h = runPool $ HS.statement h $ V.toList <$> getFullChoresByHousehold
