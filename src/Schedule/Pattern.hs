{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Schedule.Pattern
  --( WeeklyPattern
  --, MonthlyPattern
  --, WeeklyPatternState
  --, MonthlyPatternState
  --) where
  where

import Data.Set (Set, elemAt, size)
import Data.Time.Calendar hiding (DayOfMonth)
import Data.List (unfoldr)
import Schedule.Primitives
import Data.Bool

data Pattern a = Pattern
  -- ^ set of (iteration index, position in iteration)
  { _elems :: Set (Int, a)
  -- ^ size of iteration (week/month)
  , _iterations :: Int
  } deriving (Eq, Show)

type WeeklyPattern = Pattern Weekday
type MonthlyPattern = Pattern DayOfMonth

data PatternState a = PatternState (Pattern a) PatternPosition deriving (Eq, Show)
type WeeklyPatternState = PatternState Weekday
type MonthlyPatternState = PatternState DayOfMonth

data PatternPosition = PatternPosition
  { _day :: Day
  , _index :: Int
  } deriving (Eq, Show)

data PatternStateError
  = IndexOutOfRange
  | DayInvalid

nextWeekDay :: Day -> (Int, Weekday) -> (Int, Weekday) -> Day
nextWeekDay startDay startElem nextElem =
  addDays (fromIntegral $ weekOffset + dayOffset) startDay
  where
    weekOffset = 7 * (fst nextElem - fst startElem)
    dayOffset = fromEnum (snd nextElem) - fromEnum (snd startElem)

nextMonthDay :: Day -> (Int, DayOfMonth) -> (Int, DayOfMonth) -> Day
nextMonthDay startDay startElem nextElem =
  fromGregorian newYear newMonth newDay
  where
    monthOffset = fst nextElem - fst startElem
    newDay = unDayOfMonth $ snd nextElem
    (newYear, newMonth, _) =
      toGregorian $ addGregorianMonthsClip (fromIntegral monthOffset) startDay

dupe :: a -> (a, a)
dupe x = (x, x)

-------------------------------------------------------------------------------

elemAtEither :: Int -> Set a -> Either PatternStateError a
elemAtEither index set =
  if index < size set
  then Right $ elemAt index set
  else Left IndexOutOfRange

validatePatternState
  :: Eq a
  => (Day -> a)
  -> Pattern a
  -> PatternPosition
  -> Either PatternStateError (PatternState a)
validatePatternState f pat@Pattern{..} pos@PatternPosition {..} =
  bool (Left DayInvalid) (Right $ PatternState pat pos) .
  (f _day ==) . snd =<< elemAtEither _index _elems

validateWeeklyState
  :: WeeklyPattern
  -> PatternPosition
  -> Either PatternStateError WeeklyPatternState
validateWeeklyState = validatePatternState getWeekday

validateMonthlyState
  :: MonthlyPattern
  -> PatternPosition
  -> Either PatternStateError MonthlyPatternState
validateMonthlyState = validatePatternState getDayOfMonth

nextPosition
  :: (Day -> (Int, a) -> (Int, a) -> Day)
  -> PatternState a
  -> PatternState a
nextPosition f (PatternState pat@Pattern{..} (PatternPosition {..})) =
  PatternState pat $ PatternPosition nextDay nextIndex
  where
    nextIndex = if _index == size _elems - 1 then 0 else _index + 1
    nextDay = f _day (elemAt _index _elems) (elemAt nextIndex _elems)

nextPositionWeekly :: WeeklyPatternState -> WeeklyPatternState
nextPositionWeekly = nextPosition nextWeekDay

nextPositionMonthly :: MonthlyPatternState -> MonthlyPatternState
nextPositionMonthly = nextPosition nextMonthDay

nextDays :: (PatternState a -> PatternState a) -> PatternState a -> [PatternState a]
nextDays f = unfoldr (Just . dupe . f)
  --where getDay (PatternState _ p') = _day p'

nextDaysWeekly :: WeeklyPatternState -> [WeeklyPatternState]
nextDaysWeekly = nextDays nextPositionWeekly

nextDaysMonthly :: MonthlyPatternState -> [MonthlyPatternState]
nextDaysMonthly = nextDays nextPositionMonthly

nextEligibleDay
  :: Eq a
  => (Day -> a)
  -> Pattern a
  -> Int
  -> Day
  -> Either PatternStateError (PatternState a)
nextEligibleDay f p@Pattern{_elems} i d =
  constr . findDay <$> elemM
  where
    elemM = elemAtEither i _elems
    constr = PatternState p . flip PatternPosition i
    findDay (_,a) = head $ filter ((a==) . f) [d..]

nextEligibleDayWeekly
  :: WeeklyPattern
  -> Int
  -> Day
  -> Either PatternStateError WeeklyPatternState
nextEligibleDayWeekly = nextEligibleDay getWeekday

nextEligibleDayMonthly
  :: MonthlyPattern
  -> Int
  -> Day
  -> Either PatternStateError MonthlyPatternState
nextEligibleDayMonthly = nextEligibleDay getDayOfMonth
