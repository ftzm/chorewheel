{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Chore where

import Data.Time.Clock
import Data.Time.Calendar
import Data.UUID
import Data.Generics.Internal.VL.Lens
import Data.Generics.Labels() --instance declarations

import Schedule

-------------------------------------------------------------------------------
-- | Represents a period of time. The first date is guaranteed to be before the
-- second date.
newtype Period = UTCTime UTCTime

newtype ChoreId = ChoreId { unChoreId :: UUID} deriving (Eq, Show)

data Chore = Chore
  { id' :: ChoreId
  , name :: Text
  , schedule :: ScheduleState
  , lastResolution :: Maybe Resolution
  } deriving (Eq, Show, Generic)

data ChoreEvent
  -- | Complete a task
  = Complete UTCTime
  -- | Skip a task. Has the same effect as completing the task in that the
  -- task is subsequently scheduled for the next logical day, but makes the
  -- distinction that the task was not actually completed.
  | Skip UTCTime

data ChoreStatus
  -- | the chore is scheduled some time in the future.
  = NotDue
  -- | the chore is scheduled for today
  | Due
  -- | the chore was scheduled for before today, and between that day and
  -- today the chore should have been scheduled n additional times.
  | Overdue Int

newtype Scheduled = Scheduled { unScheduled :: Day}
newtype Resolved = Resolved { unResolved :: Day}

-- where resolutionDay is later than the previous scheduled, return the lapsed
-- days where they exist, and the resolution fed in, in a list.
doChore
  :: Chore
  -> Resolution
  -> Either ResolutionError ([Resolution], Chore)
doChore c@Chore {..} resolution =
  resolveSchedule schedule ((.day) <$> lastResolution) resolution <&>
    \(resolutions, nextScheduleState) ->
      ( resolutions
      , c & #schedule .~ nextScheduleState
          & #lastResolution .~ (Just resolution)
      )
