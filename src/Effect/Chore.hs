{-# LANGUAGE UndecidableInstances #-}


module Effect.Chore where

import qualified Hasql.Session as HS
import qualified Data.Vector as V
import Control.Monad.Catch

import Models
import Chore
import Schedule
import DB
import DB.Chore
import DB.Schedule
import DB.Household

getFullChoresImpl :: (WithDb r m) => HouseholdId -> m [Chore]
getFullChoresImpl h = runPool $ HS.statement h $ V.toList <$> getFullChoresByHousehold

saveChoreImpl :: (WithDb r m) => UserId -> HouseholdId -> Chore -> m ()
saveChoreImpl userId householdId chore = runPool $ do
  isHouseholdMember' <- HS.statement (userId, householdId) isHouseholdMember
  if isHouseholdMember'
  then do
    HS.statement (householdId, chore.id', chore.name) insertChore
    insertSchedule (chore.id', chore.schedule)
  else
    error "User is not a household member"

resolveChoreImpl :: (WithDb r m) => UserId -> HouseholdId -> Chore -> Resolution -> m ()
resolveChoreImpl _ _ c r = do
  (rs, ss) <- either (error . show) pure
    $ resolveSchedule c.schedule ((.day) <$> c.lastResolution) r
  runPool $ do
    HS.statement (V.fromList $ map (c.id',) rs) insertChoreEvents
    updateSchedule' (c.id', ss)

class Monad m => ChoreM m where
  getFullChores :: HouseholdId -> m [Chore]
  saveChore :: UserId -> HouseholdId -> Chore -> m ()
  resolveChore :: UserId -> HouseholdId -> Chore -> Resolution -> m ()

newtype ChoreT m a = ChoreT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadThrow)

instance (WithDb r m) => ChoreM (ChoreT m) where
  getFullChores = getFullChoresImpl
  saveChore = saveChoreImpl
  resolveChore = resolveChoreImpl
