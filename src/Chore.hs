{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- TypeNats experiment
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Chore where

import Data.Time.Clock
import Data.Time.Calendar

-- data Schedule
--   -- ^ Schedule next task n days since the task was completed, even if late.
--   = FlexDays Int
--   -- ^ Schedule tasks n days apart.
--   | StrictDays Int
--   | WeeklyPattern (Pattern Weekday)
--   | MonthlyPattern (Pattern DayOfMonth)
--   --  | StrictYear (Set YearDate) Interval

-- data ScheduleState
--   = FlexDayState Int Day
--   | StrictDayState Int Day
--   | WeeklyPatternState (PatternState Weekday)
--   | MonthlyPatternState (PatternState DayOfMonth)

-- data ScheduleStateConstructionError
--   = PositionOutOfRange
--   | DayInvalid

-- nextDayFromScheduleState :: ScheduleState -> ScheduleState
-- nextDayFromScheduleState s = case s of
--   FlexDayState i d -> FlexDayState i $ findNextFlexDay d i
--   StrictDayState i d -> StrictDayState i $ findNextStrictDay d i
--   WeeklyPatternState ps -> WeeklyPatternState $ weekPatternStateStep ps
--   MonthlyPatternState ps  -> MonthlyPatternState $ monthPatternStateStep ps

-- findNextFlexDay :: Day -> Int -> Day
-- findNextFlexDay = undefined

-- findNextStrictDay :: Day -> Int -> Day
-- findNextStrictDay = undefined

-------------------------------------------------------------------------------
-- | Represents a period of time. The first date is guaranteed to be before the
-- second date.
newtype Period = UTCTime UTCTime

data Resolution
  = Completed UTCTime
  | Cancelled UTCTime
  | Rescheduled UTCTime

data ChoreEvent
  -- | Complete a task
  = Complete UTCTime
  -- | Cancel a task. Has the same effect as completing the task in that the
  -- task is subsequently scheduled for the next logical day, but makes the
  -- distinction that the task was not actually completed.
  | Cancel UTCTime
  -- | Apply a pause period to a task. This has the effect of moving an
  -- existing or newly scheduled task to after the pause period has ended.
  | Pause Period

data ChoreStatus
  -- | the chore is scheduled for today
  = Due
  -- | the chore is scheduled for before today, but the next logical
  -- occurrence is after today
  | Overdue
  -- | The chore was not done on its scheduled day, and between that day and
  -- today the chore should have been scheduled n additional times.
  | Skipped Int

-- data ChoreState = ChoreState
--   -- | the first scheduled occurrence of this task. This represents either the
--   -- first scheduled instance of a task ever, or the first scheduled instance
--   -- of a task since after if was last completed.
--   { firstScheduled :: Day
--   , schedule :: Schedule
--   }

newtype Scheduled = Scheduled { unScheduled :: Day}
newtype Resolved = Resolved { unResolved :: Day}


-- Necessary operations

-- create schedule
-- show schedule
-- edit schedule

-- set first schedule day
-- get all schedule days into the future

-- non destructive schedule edits form schedules with past days associated
--
