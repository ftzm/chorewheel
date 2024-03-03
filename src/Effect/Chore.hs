{-# LANGUAGE UndecidableInstances #-}

module Effect.Chore where

import Control.Monad.Catch
import Data.Map qualified as M
import Data.Time.Calendar (Day)
import Data.Vector qualified as V
import Hasql.Session qualified as HS

import Chore
import DB
import DB.Chore
import DB.Household
import DB.Schedule
import Effect.Household
import Models
import Schedule

getFullChoresImpl :: (WithDb r m) => HouseholdId -> m [Chore]
getFullChoresImpl h =
  runPool $ HS.statement h $ V.toList <$> getFullChoresByHousehold

getFullChoreImpl :: (WithDb r m) => ChoreId -> m Chore
getFullChoreImpl h = runPool $ HS.statement h getFullChoreById

saveChoreImpl' :: (WithDb r m) => HouseholdMembership -> Chore -> m ()
saveChoreImpl' membership chore = runPool $ do
  HS.statement (membership.householdId, chore.id, chore.name) insertChore
  insertSchedule (chore.id, chore.schedule)
  insertParticipants (chore.id, chore.participants)

saveChoreImpl :: (WithDb r m) => UserId -> HouseholdId -> Chore -> m ()
saveChoreImpl userId householdId chore = runPool $ do
  isHouseholdMember' <- HS.statement (userId, householdId) isHouseholdMemberQ
  if isHouseholdMember'
    then do
      HS.statement (householdId, chore.id, chore.name) insertChore
      insertSchedule (chore.id, chore.schedule)
      insertParticipants (chore.id, chore.participants)
    else error "User is not a household member"

resolveChoreImpl ::
  (WithDb r m) => UserId -> HouseholdId -> Chore -> Resolution -> m ()
resolveChoreImpl _ _ c r = do
  (rs, ss) <-
    either (error . show) pure $
      resolveSchedule c.schedule ((.day) <$> c.lastResolution) r
  runPool $ do
    HS.statement (V.fromList $ map (c.id,) rs) insertChoreEventsQ
    updateSchedule' (c.id, ss)

allChoreEventsImpl ::
  (WithDb r m) =>
  UserId ->
  HouseholdId ->
  Day ->
  Day ->
  m (M.Map ChoreId [Resolution])
allChoreEventsImpl _ hId from to = do
  runPool $ HS.statement (hId, from, to) getHouseholdChoreEventsFromTo

choreEventsImpl ::
  (WithDb r m) =>
  UserId ->
  HouseholdId ->
  ChoreId ->
  Day ->
  Day ->
  m (V.Vector Resolution)
choreEventsImpl _ _ cId from to = do
  runPool $ HS.statement (cId, from, to) getChoreEventsFromTo

getLatestChoreEventImpl ::
  (WithDb r m) =>
  UserId ->
  HouseholdId ->
  ChoreId ->
  m (Maybe Resolution)
getLatestChoreEventImpl _ _ cId =
  runPool $ HS.statement cId getLatestChoreEventQ

deleteChoreEventImpl ::
  (WithDb r m) =>
  UserId ->
  ChoreId ->
  Day ->
  m ()
deleteChoreEventImpl _ cId day =
  runPool $ HS.statement (cId, day) deleteChoreEventQ

insertChoreEventsImpl ::
  (WithDb r m) =>
  UserId ->
  HouseholdId ->
  V.Vector (ChoreId, Resolution) ->
  m ()
insertChoreEventsImpl _ _ es =
  runPool $ HS.statement es insertChoreEventsQ

updateScheduleImpl ::
  (WithDb r m) =>
  UserId ->
  ChoreId ->
  ScheduleState ->
  m ()
updateScheduleImpl _ cId ss =
  runPool $ updateSchedule' (cId, ss)

class Monad m => ChoreM m where
  getFullChores :: HouseholdId -> m [Chore]
  getFullChore :: ChoreId -> m Chore
  saveChore :: UserId -> HouseholdId -> Chore -> m ()
  resolveChore :: UserId -> HouseholdId -> Chore -> Resolution -> m ()
  allChoreEvents ::
    UserId -> HouseholdId -> Day -> Day -> m (M.Map ChoreId [Resolution])
  choreEvents ::
    UserId -> HouseholdId -> ChoreId -> Day -> Day -> m (V.Vector Resolution)
  insertChoreEvents ::
    UserId -> HouseholdId -> V.Vector (ChoreId, Resolution) -> m ()
  deleteChoreEvent :: UserId -> ChoreId -> Day -> m ()
  getLatestChoreEvent ::
    UserId -> HouseholdId -> ChoreId -> m (Maybe Resolution)
  updateSchedule ::
    UserId -> ChoreId -> ScheduleState -> m ()

newtype ChoreT m a = ChoreT (m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader r
    , MonadThrow
    )

instance (WithDb r m) => ChoreM (ChoreT m) where
  getFullChores = getFullChoresImpl
  getFullChore = getFullChoreImpl
  saveChore = saveChoreImpl
  resolveChore = resolveChoreImpl
  allChoreEvents = allChoreEventsImpl
  choreEvents = choreEventsImpl
  deleteChoreEvent = deleteChoreEventImpl
  getLatestChoreEvent = getLatestChoreEventImpl
  insertChoreEvents = insertChoreEventsImpl
  updateSchedule = updateScheduleImpl
