{-# LANGUAGE UndecidableInstances #-}

module Effect.Chore where

import qualified Hasql.Session as HS
import qualified Data.Vector as V

import Models
import Chore
import DB
import DB.Chore
import DB.Schedule
import DB.Household

saveChoreImpl :: (WithDb r m) => UserId -> HouseholdId -> Chore -> m ()
saveChoreImpl userId householdId chore = runPool $ do
  isHouseholdMember' <- HS.statement (userId, householdId) isHouseholdMember
  if isHouseholdMember'
  then do
    HS.statement (householdId, chore.id', chore.name) insertChore
    insertSchedule (chore.id', chore.schedule)
  else
    error "User is not a household member"


class Monad m => ChoreM m where
  getFullChores :: HouseholdId -> m [Chore]
  saveChore :: UserId -> HouseholdId -> Chore -> m ()

  --resolveChore :: Chore -> Resolution -> m (Either ResolutionError Chore)

newtype ChoreT m a = ChoreT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance (WithDb r m) => ChoreM (ChoreT m) where
  getFullChores h = runPool $ HS.statement h $ V.toList <$> getFullChoresByHousehold
  saveChore = saveChoreImpl
  --resolveChore chore resolution = undefined
    -- case doChore chore resolution of
    --   Right (lapsed, nextChore) -> undefined
    --   Left e -> return $ Left e
