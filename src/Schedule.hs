module Schedule where

import Data.Time.Calendar (Day, addDays)
import Data.UUID

import Schedule.Pattern

newtype ScheduleId = ScheduleId {unScheduleId :: UUID}

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

data FlexDaysState = FlexDaysState FlexDays Day
   deriving (Eq, Show)
data StrictDaysState = StrictDaysState StrictDays Day
   deriving (Eq, Show)

data ScheduleState
  -- ^ Schedule next task n days since the task was completed, even if late.
  -- Next scheduled day not needed because we just count from the last
  -- completed day. If none, consider it scheduled for today.
  = FlexDaysSS FlexDaysState
  -- ^ Schedule tasks n days apart.
  -- Include the next scheduled day.
  | StrictDaysSS StrictDaysState
  -- ^ Schedule tasks by a repeating pattern of weeks/months.
  -- the included pattern state is the last scheduled day.
  | WeeklyPatternSS WeeklyPatternState
  | MonthlyPatternSS MonthlyPatternState
   deriving (Eq, Show)

scheduleStateDay :: ScheduleState -> Day
scheduleStateDay (FlexDaysSS (FlexDaysState _ d)) = d
scheduleStateDay (StrictDaysSS (StrictDaysState _ d)) = d
scheduleStateDay (WeeklyPatternSS (PatternState _ PatternPosition {day})) = day
scheduleStateDay (MonthlyPatternSS (PatternState _ PatternPosition {day})) = day

-- data ScheduleStateUpdate
--   = FlexDaysU Day
--   | StrictDaysU Day
--   | PatternStateU PatternPosition

-- It's not clear that I'll ever have use for a schedule alone withouth the
-- state, as the state is essentially always relevant when creating or
-- updating. It may make sense to only have a scheduleState-like type where the
-- state is optional

nextDaysFlex :: FlexDaysState -> [FlexDaysState]
nextDaysFlex =
  unfoldr (\(FlexDaysState s@(FlexDays i) day) ->
             Just $ dupe $ FlexDaysState s $ addDays (fromIntegral i) day)

nextDaysStrict :: StrictDaysState -> [StrictDaysState]
nextDaysStrict =
  unfoldr (\(StrictDaysState s@(StrictDays i) day) ->
             Just $ dupe $ StrictDaysState s $ addDays (fromIntegral i) day)

resolveFlexDays :: FlexDaysState -> Day -> ([Day], Maybe FlexDaysState)
resolveFlexDays f@(FlexDaysState s@(FlexDays days) dayScheduled) dayResolved
  | dayScheduled > dayResolved =
    ([], Just $ FlexDaysState s $ addDays (fromIntegral days) dayResolved)
  | otherwise =
    ( takeWhile (dayResolved>) $ map getDayFlex $ f : nextDaysFlex f
    , Just $ FlexDaysState s $ addDays (fromIntegral days) dayResolved
    )
  where
    getDayFlex (FlexDaysState _ d) = d

resolveStrictDays :: StrictDaysState -> Day -> ([Day], Maybe StrictDaysState)
resolveStrictDays s'@(StrictDaysState _ dayScheduled) dayResolved
  | dayScheduled > dayResolved = ([], Nothing)
  | otherwise = bimap (map getDayStrict) (viaNonEmpty head)
                $ span ((dayResolved>) . getDayStrict) $ s' : nextDaysStrict s'
  where
    getDayStrict (StrictDaysState _ d) = d
