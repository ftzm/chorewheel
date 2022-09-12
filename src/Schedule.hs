{-# LANGUAGE PatternSynonyms #-}

module Schedule
  ( Schedule (..)
  , ScheduleState (..)
  , ResolutionType (..)
  , Resolution (..)
  , ResolutionError (..)
  , FlexDaysState (..)
  , FlexDays (..)
  , StrictDaysState (..)
  , StrictDays (..)
  , resolveSchedule
  , futureStates
  , PosNeg (..)
  , pattern Smarter
  , nonneg
  , pattern Right1
  ) where

import Data.Time.Calendar (Day, addDays)

import Models
import Schedule.Pattern
import Schedule.Primitives

-- | Schedule next task n days since the task was completed, even if late.
newtype FlexDays = FlexDays { unFlexDays :: Int } deriving (Eq, Show)

-- mkFlexDays :: Int -> FlexDays
-- mkFlexDays = undefined

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


data ResolutionError
  = InvalidDay
  deriving (Eq, Show)

data ResolutionType
  -- Complete task, self-explanatory.
  = Completed UserId
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

-- scheduleStateDay :: ScheduleState -> Day
-- scheduleStateDay (FlexDaysSS (FlexDaysState _ d)) = d
-- scheduleStateDay (StrictDaysSS (StrictDaysState _ d)) = d
-- scheduleStateDay (WeeklyPatternSS (PatternState _ PatternPosition {day})) = day
-- scheduleStateDay (MonthlyPatternSS (PatternState _ PatternPosition {day})) = day

-- data ScheduleStateUpdate
--   = FlexDaysU Day
--   | StrictDaysU Day
--   | PatternStateU PatternPosition

-- It's not clear that I'll ever have use for a schedule alone withouth the
-- state, as the state is essentially always relevant when creating or
-- updating. It may make sense to only have a scheduleState-like type where the
-- state is optional

futureStatesFlex :: FlexDaysState -> [FlexDaysState]
futureStatesFlex =
  unfoldr (\(FlexDaysState s@(FlexDays i) day) ->
             Just $ dupe $ FlexDaysState s $ addDays (fromIntegral i) day)

futureStatesStrict :: StrictDaysState -> [StrictDaysState]
futureStatesStrict =
  unfoldr (\(StrictDaysState s@(StrictDays i) day) ->
             Just $ dupe $ StrictDaysState s $ addDays (fromIntegral i) day)

resolveFlexDays :: FlexDaysState -> Day -> ([Day], Maybe FlexDaysState)
resolveFlexDays f@(FlexDaysState s@(FlexDays days) dayScheduled) dayResolved
  | dayScheduled > dayResolved =
    ([], Just $ FlexDaysState s $ addDays (fromIntegral days) dayResolved)
  | otherwise =
    ( takeWhile (dayResolved>) $ map getDayFlex $ f : futureStatesFlex f
    , Just $ FlexDaysState s $ addDays (fromIntegral days) dayResolved
    )
  where
    getDayFlex (FlexDaysState _ d) = d

resolveStrictDays :: StrictDaysState -> Day -> ([Day], Maybe StrictDaysState)
resolveStrictDays s'@(StrictDaysState _ dayScheduled) dayResolved
  | dayScheduled > dayResolved = ([], Nothing)
  | otherwise = bimap (map getDayStrict) (viaNonEmpty head)
                $ span ((dayResolved>) . getDayStrict) $ s' : futureStatesStrict s'
  where
    getDayStrict (StrictDaysState _ d) = d

resolveSchedule
  :: ScheduleState
  -> Maybe Day -- ^ Last resolved day
  -> Resolution -- ^ The new resolution
  -> Either ResolutionError ([Resolution], ScheduleState)
resolveSchedule scheduleState lastResolved resolution
  | maybe False (>= resolution.day) lastResolved = Left InvalidDay
  | otherwise =
    let
      (lapsedDays, nextScheduleState) =
        case scheduleState of
          FlexDaysSS s ->
            fmap FlexDaysSS <$> resolveFlexDays s resolution.day
          StrictDaysSS s ->
            fmap StrictDaysSS <$> resolveStrictDays s resolution.day
          WeeklyPatternSS s ->
            fmap WeeklyPatternSS <$> resolvePatternWeekly s resolution.day
          MonthlyPatternSS s ->
            fmap MonthlyPatternSS <$> resolvePatternMonthly s resolution.day
      lapses = map (flip Resolution Lapsed) lapsedDays
      updatedScheduleState = fromMaybe scheduleState nextScheduleState
    in Right (lapses ++ [resolution], updatedScheduleState)

futureStates :: ScheduleState -> [ScheduleState]
futureStates (FlexDaysSS s) = map FlexDaysSS $ futureStatesFlex s
futureStates (StrictDaysSS s) = map StrictDaysSS $ futureStatesStrict s
futureStates (WeeklyPatternSS s) = map WeeklyPatternSS $ futureStatesWeekly s
futureStates (MonthlyPatternSS s) = map MonthlyPatternSS $ futureStatesMonthly s

---
-- I could use pattern synonyms to hide the constructors

data PosNeg = Pos Int | Neg Int

pattern Smarter :: Int -> PosNeg
pattern Smarter{ nonneg } <- Pos nonneg  where
  Smarter x | x >= 0    = (Pos x)
            | otherwise = (Neg x)

pattern Right1 :: Int -> Either String Int
pattern Right1 i <- Right i where
  Right1 x | 1 == x = Right 1
           | otherwise = Left "shit"
