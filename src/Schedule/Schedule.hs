module Schedule where

import Schedule.Pattern

data Schedule
  -- ^ Schedule next task n days since the task was completed, even if late.
  = FlexDays Int
  -- ^ Schedule tasks n days apart.
  | StrictDays Int
  -- ^ Schedule tasks by a repeating pattern of weeks/months.
  | Pattern Pattern

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

-- Necessary operations

-- create schedule
-- show schedule
-- edit schedule

-- set first schedule day
-- get all schedule days into the future

-- non destructive schedule edits form schedules with past days associated
--
