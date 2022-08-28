{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}

module Chore where

import Data.Time.Clock
import Data.Time.Calendar
import Data.UUID
import Data.Generics.Internal.VL.Lens
import Data.Generics.Labels() --instance declarations

import Schedule
import Schedule.Pattern

-------------------------------------------------------------------------------
-- | Represents a period of time. The first date is guaranteed to be before the
-- second date.
newtype Period = UTCTime UTCTime

data ResolutionError
  = InvalidDay
  deriving (Eq, Show)

data ResolutionType
  -- Complete task, self-explanatory.
  = Completed
  -- record originally scheduled day that was deliberately skipped, causing the
  -- task to be scheduled in the future as though it was completed.
  | Skipped
  -- record originally scheduled day that was passed
  | Lapsed
  deriving (Eq, Show)

data Resolution = Resolution
  { day :: Day
  , resolutionType :: ResolutionType
  } deriving (Eq, Show)

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
doChore c@Chore {..} resolution
  | maybe False ((>= resolution.day) . (.day)) lastResolution = Left InvalidDay
  | otherwise =
    let
      (lapsedDays, nextScheduleState) =
        case schedule of
          FlexDaysSS s ->
            fmap FlexDaysSS <$> resolveFlexDays s resolution.day
          StrictDaysSS s ->
            fmap StrictDaysSS <$> resolveStrictDays s resolution.day
          WeeklyPatternSS s ->
            fmap WeeklyPatternSS <$> resolvePatternWeekly s resolution.day
          MonthlyPatternSS s ->
            fmap MonthlyPatternSS <$> resolvePatternMonthly s resolution.day
      lapses = map (flip Resolution Lapsed) lapsedDays
      updatedScheduleState = fromMaybe schedule nextScheduleState
      nextChore =
        c & #schedule .~ updatedScheduleState & #lastResolution .~ Just resolution
    in Right (lapses ++ [resolution], nextChore)
