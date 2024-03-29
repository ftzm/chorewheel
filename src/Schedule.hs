-- {-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}

module Schedule (
  Schedule (..),
  ScheduleState (..),
  ResolutionType (..),
  Resolution (..),
  ResolutionError (..),
  FlexDaysState (..),
  FlexDays (..),
  StrictDaysState (..),
  StrictDays (..),
  resolveSchedule,
  futureStates,
  nextEligibleDay,
  scheduleStateWindow,
  ssDay,
  reverseUntil,
  -- PosNeg (..),
  -- pattern Smarter,
  -- nonneg,
  -- pattern Right1,
) where

import Data.Time.Calendar (Day, addDays)

import Models
import Schedule.Pattern
import Schedule.Primitives

-- import GHC.Records

-- | Schedule next task n days since the task was completed, even if late.
newtype FlexDays = FlexDays {unFlexDays :: Int} deriving (Eq, Show)

-- mkFlexDays :: Int -> FlexDays
-- mkFlexDays = undefined

-- | Schedule tasks n days apart.
newtype StrictDays = StrictDays {unStrictDays :: Int} deriving (Eq, Show)

data Schedule
  = FlexDaysS FlexDays
  | -- | Schedule tasks by a repeating pattern of weeks/months.
    StrictDaysS StrictDays
  | WeeklyPatternS WeeklyPattern
  | MonthlyPatternS MonthlyPattern
  | UnscheduledS
  deriving (Eq, Show)

data FlexDaysState = FlexDaysState FlexDays Day
  deriving (Eq, Show)
data StrictDaysState = StrictDaysState StrictDays Day
  deriving (Eq, Show)

data ResolutionError
  = InvalidDay
  deriving (Eq, Show)

data ResolutionType
  = -- Complete task, self-explanatory.
    Completed UserId
  | -- record originally scheduled day that was deliberately skipped, causing the
    -- task to be scheduled in the future as though it was completed.
    Skipped
  | -- record originally scheduled day that was passed
    Lapsed
  deriving (Eq, Show)

data Resolution = Resolution
  { day :: Day
  , resolutionType :: ResolutionType
  }
  deriving (Eq, Show)

data ScheduleState
  = -- \^ Schedule next task n days since the task was completed, even if late.
    -- Next scheduled day not needed because we just count from the last
    -- completed day. If none, consider it scheduled for today.

    -- | Schedule tasks n days apart.
    -- Include the next scheduled day.
    FlexDaysSS FlexDaysState
  | -- | Schedule tasks by a repeating pattern of weeks/months.
    -- the included pattern state is the last scheduled day.
    StrictDaysSS StrictDaysState
  | WeeklyPatternSS WeeklyPatternState
  | MonthlyPatternSS MonthlyPatternState
  | UnscheduledSS
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
  unfoldr
    ( \(FlexDaysState s@(FlexDays i) day) ->
        Just $ dupe $ FlexDaysState s $ addDays (fromIntegral i) day
    )

futureStatesStrict :: StrictDaysState -> [StrictDaysState]
futureStatesStrict =
  unfoldr
    ( \(StrictDaysState s@(StrictDays i) day) ->
        Just $ dupe $ StrictDaysState s $ addDays (fromIntegral i) day
    )

pastStatesStrict :: StrictDaysState -> [StrictDaysState]
pastStatesStrict =
  unfoldr
    ( \(StrictDaysState s@(StrictDays i) day) ->
        Just $ dupe $ StrictDaysState s $ addDays (fromIntegral (-i)) day
    )

resolveFlexDays :: FlexDaysState -> Day -> ([Day], Maybe FlexDaysState)
resolveFlexDays f@(FlexDaysState s@(FlexDays days) dayScheduled) dayResolved
  | dayScheduled > dayResolved =
      ([], Just $ FlexDaysState s $ addDays (fromIntegral days) dayResolved)
  | otherwise =
      ( takeWhile (dayResolved >) $ map getDayFlex $ f : futureStatesFlex f
      , Just $ FlexDaysState s $ addDays (fromIntegral days) dayResolved
      )

getDayStrict :: StrictDaysState -> Day
getDayStrict (StrictDaysState _ d) = d

getDayFlex :: FlexDaysState -> Day
getDayFlex (FlexDaysState _ d) = d

resolveStrictDays :: StrictDaysState -> Day -> ([Day], Maybe StrictDaysState)
resolveStrictDays s'@(StrictDaysState _ dayScheduled) dayResolved
  | dayScheduled > dayResolved = ([], Nothing)
  | otherwise =
      bimap (map getDayStrict) (viaNonEmpty head . dropWhile ((dayResolved >=) . getDayStrict)) $
        span ((dayResolved >) . getDayStrict) $
          s' : futureStatesStrict s'

resolveSchedule ::
  ScheduleState ->
  -- | Last resolved day
  Maybe Day ->
  -- | The new resolution
  Resolution ->
  Either ResolutionError ([Resolution], ScheduleState)
resolveSchedule scheduleState lastResolved resolution
  | maybe False (>= resolution.day) lastResolved = Left InvalidDay
  | otherwise =
      let (lapsedDays, nextScheduleState) =
            case scheduleState of
              FlexDaysSS s ->
                fmap FlexDaysSS <$> resolveFlexDays s resolution.day
              StrictDaysSS s ->
                fmap StrictDaysSS <$> resolveStrictDays s resolution.day
              WeeklyPatternSS s ->
                fmap WeeklyPatternSS <$> resolvePatternWeekly s resolution.day
              MonthlyPatternSS s ->
                fmap MonthlyPatternSS <$> resolvePatternMonthly s resolution.day
              UnscheduledSS -> ([], Nothing)
          lapses = map (`Resolution` Lapsed) lapsedDays
          updatedScheduleState = fromMaybe scheduleState nextScheduleState
       in Right (lapses ++ [resolution], updatedScheduleState)

futureStates :: ScheduleState -> [ScheduleState]
futureStates (FlexDaysSS s) = map FlexDaysSS $ futureStatesFlex s
futureStates (StrictDaysSS s) = map StrictDaysSS $ futureStatesStrict s
futureStates (WeeklyPatternSS s) = map WeeklyPatternSS $ futureStatesWeekly s
futureStates (MonthlyPatternSS s) = map MonthlyPatternSS $ futureStatesMonthly s
futureStates UnscheduledSS = []

nextEligibleDay :: Day -> Schedule -> Either PatternStateError ScheduleState
nextEligibleDay day (FlexDaysS s) = return $ FlexDaysSS $ FlexDaysState s day
nextEligibleDay day (StrictDaysS s) = return $ StrictDaysSS $ StrictDaysState s day
nextEligibleDay day (WeeklyPatternS s) = WeeklyPatternSS <$> nextEligibleDayWeekly s 0 day
nextEligibleDay day (MonthlyPatternS s) = MonthlyPatternSS <$> nextEligibleDayMonthly s 0 day
nextEligibleDay _ _ = return UnscheduledSS

ssDay :: ScheduleState -> Maybe Day
ssDay (FlexDaysSS (FlexDaysState _ d)) = Just d
ssDay (StrictDaysSS (StrictDaysState _ d)) = Just d
ssDay (WeeklyPatternSS wp) = Just $ getDay wp
ssDay (MonthlyPatternSS wp) = Just $ getDay wp
ssDay UnscheduledSS = Nothing

scheduleStateWindow :: Day -> Day -> ScheduleState -> [ScheduleState]
scheduleStateWindow from until s =
  takeWhile (maybe False (<= until) . ssDay) $
    dropWhile (maybe False (from >) . ssDay) $
      s : futureStates s

-- given a schedule state and a day in the past, get the earliest possible
-- schedule state after the day.
-- TODO: argument should be a day+schedule state pair that guarantees that the
-- day is before the scheduleState
reverseUntil :: Day -> ScheduleState -> Maybe ScheduleState
reverseUntil d = \case
  FlexDaysSS (FlexDaysState (FlexDays n) _) ->
    Just $ FlexDaysSS $ FlexDaysState (FlexDays n) $ addDays (fromIntegral n) d
  StrictDaysSS s ->
    StrictDaysSS
      <$> viaNonEmpty
        last
        (takeWhile (\(StrictDaysState _ d') -> d' > d) (pastStatesStrict s))
  WeeklyPatternSS s ->
    WeeklyPatternSS
      <$> viaNonEmpty
        last
        ( takeWhile
            (\(PatternState _ (PatternPosition d' _)) -> d' > d)
            (pastStatesWeekly s)
        )
  MonthlyPatternSS s ->
    MonthlyPatternSS
      <$> viaNonEmpty
        last
        ( takeWhile
            (\(PatternState _ (PatternPosition d' _)) -> d' > d)
            (pastStatesMonthly s)
        )
  UnscheduledSS -> Just UnscheduledSS

---
-- I could use pattern synonyms to hide the constructors

-- data TestRec = TestRec {one :: Int, two :: String}
--   deriving Generic
--
-- pattern TestPattern :: Int -> String -> TestRec
-- pattern TestPattern{newOne, newTwo} = TestRec newOne newTwo
--
-- getOne :: TestRec -> Int
-- getOne = newOne
--
-- instance HasField "newOne" TestRec Int where
--   getField = getOne
--
-- getOne' :: TestRec -> Int
-- getOne' r = r.newOne
--
-- data PosNeg = Pos Int | Neg Int
--
-- pattern Smarter :: Int -> PosNeg
-- pattern Smarter{nonneg} <- Pos nonneg
--   where
--     Smarter x
--       | x >= 0 = Pos x
--       | otherwise = Neg x
--
-- pattern Right1 :: Int -> Either String Int
-- pattern Right1 i <- Right i
--   where
--     Right1 x
--       | 1 == x = Right 1
--       | otherwise = Left "shit"
