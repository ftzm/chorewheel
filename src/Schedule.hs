module Schedule where

import Data.Time.Calendar (Day)
import Data.Int (Int32)

import Schedule.Pattern

newtype ScheduleId = ScheduleId {unScheduleId :: Int32}

-- | Schedule next task n days since the task was completed, even if late.
newtype FlexDays = FlexDays { unFlexDays :: Int } deriving (Eq, Show)

-- | Schedule tasks n days apart.
newtype StrictDays = StrictDays { unStrictDays :: Int } deriving (Eq, Show)

data Schedule
   = FlexDaysS FlexDays
   | StrictDaysS StrictDays
   -- ^ Schedule tasks by a repeating pattern of weeks/months.
   | WeeklyPatternS WeeklyPattern
   | MonthlyPatternS MonthlyPattern
   deriving (Eq, Show)

data ScheduleState
  -- ^ Schedule next task n days since the task was completed, even if late.
  -- Next scheduled day not needed because we just count from the last
  -- completed day. If none, consider it scheduled for today.
  = FlexDaysSS FlexDays
  -- ^ Schedule tasks n days apart.
  -- Include the next scheduled day.
  | StrictDaysSS StrictDays Day
  -- ^ Schedule tasks by a repeating pattern of weeks/months.
  -- the included pattern state is the last scheduled day.
  | WeeklyPatternStateS WeeklyPatternState
  | MonthlyPatternStateS MonthlyPatternState

-- data ScheduleStateUpdate
--   = FlexDaysU Day
--   | StrictDaysU Day
--   | PatternStateU PatternPosition

-- It's not clear that I'll ever have use for a schedule alone withouth the
-- state, as the state is essentially always relevant when creating or
-- updating. It may make sense to only have a scheduleState-like type where the
-- state is optional

nextDaysFlex :: FlexDays -> Day -> [Day]
nextDaysFlex = undefined

nextDaysStrict :: StrictDays -> Day -> [Day]
nextDaysStrict = undefined

-- updateNextScheduled :: ScheduleState -> Day -> ScheduleState
-- updateNextScheduled = undefined

-- create = undefined
--
-- load = undefined
--
-- -- the tricky thing here is to update the pattern state/scheduled days where relevant.
-- update = undefined
--
-- nextDays = undefined
--
-- perform = undefined
