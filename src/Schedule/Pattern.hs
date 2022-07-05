{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Schedule.Pattern
  ( PatternSchedule(..)
  , PatternState(..)
  , PatternPosition
  , validateState
  , nextDays
  ) where

import Data.Set (Set, elemAt, size)
import Data.Time.Calendar
import Data.List (unfoldr)
import Schedule.Primitives
import Data.Bool

data PatternSchedule
  = WeeklyPattern (Pattern Weekday)
  | MonthlyPattern (Pattern DayOfMonth)

data Pattern a = Pattern
  -- ^ set of (iteration index, position in iteration)
  { _elems :: Set (Int, a)
  -- ^ size of iteration (week/month)
  , _iterations :: Int
  } deriving Show

data PatternPosition = PatternPosition
  { _day :: Day
  , _index :: Int
  } deriving Show

data PatternState = PatternState PatternSchedule PatternPosition

data PatternStateError
  = IndexOutOfRange
  | DayInvalid

validateState :: PatternSchedule -> PatternPosition -> Either PatternStateError PatternState
validateState s p@PatternPosition {..} =
  bool (Left DayInvalid) (Right $ PatternState s p) =<< case s of
    WeeklyPattern (Pattern {..}) ->
      (getWeekday _day ==) . snd <$> elemAtEither _index _elems
    MonthlyPattern (Pattern {..}) ->
      (getDayOfMonth _day ==) . snd <$> elemAtEither _index _elems
  where
    elemAtEither index set =
      if index < size set
      then Right $ elemAt index set
      else Left IndexOutOfRange

nextPosition :: PatternState -> PatternState
nextPosition (PatternState s (PatternPosition {..})) =
  PatternState s $ PatternPosition nextDay nextIndex
  where
    f :: (Day -> (Int, a) -> (Int, a) -> Day) -> Set (Int, a) -> (Day, Int)
    f f' elems =
      let nextIndex' = if _index == size elems - 1 then 0 else _index + 1
       in (f' _day (elemAt _index elems) (elemAt nextIndex elems), nextIndex')
    (nextDay, nextIndex) = case s of
      WeeklyPattern (Pattern {..}) -> f nextWeekDay _elems
      MonthlyPattern (Pattern {..}) -> f nextMonthDay _elems

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

nextDays :: PatternState -> [Day]
nextDays = map getDay . unfoldr (Just . dupe . nextPosition)
  where
    getDay (PatternState _ p) = _day p

-------------------------------------------------------------------------------

type WeeklyPattern = Pattern Weekday
type MonthlyPattern = Pattern DayOfMonth

data PatternState' a = PatternState' (Pattern a) PatternPosition
type WeeklyPatternState = PatternState' Weekday
type MonthlyPatternState = PatternState' DayOfMonth

validatePatternState'
  :: Eq a
  => Pattern a
  -> PatternPosition
  -> (Day -> a)
  -> Either PatternStateError (PatternState' a)
validatePatternState' pat@Pattern{..} pos@PatternPosition {..} f =
  bool (Left DayInvalid) (Right $ PatternState' pat pos) .
  (f _day ==) . snd =<< elemAtEither _index _elems
  where
    elemAtEither index set =
      if index < size set
      then Right $ elemAt index set
      else Left IndexOutOfRange

validateWeeklyState
  :: WeeklyPattern
  -> PatternPosition
  -> Either PatternStateError WeeklyPatternState
validateWeeklyState pat pos = validatePatternState' pat pos getWeekday

validateMonthlyState
  :: MonthlyPattern
  -> PatternPosition
  -> Either PatternStateError MonthlyPatternState
validateMonthlyState pat pos = validatePatternState' pat pos getDayOfMonth

nextPosition'
  :: PatternState' a
  -> (Day -> (Int, a) -> (Int, a) -> Day)
  -> PatternState' a
nextPosition' (PatternState' pat@Pattern{..} (PatternPosition {..})) f =
  PatternState' pat $ PatternPosition nextDay nextIndex
  where
    nextIndex = if _index == size _elems - 1 then 0 else _index + 1
    nextDay = f _day (elemAt _index _elems) (elemAt nextIndex _elems)

nextPositionWeekly' :: WeeklyPatternState -> WeeklyPatternState
nextPositionWeekly' p = nextPosition' p nextWeekDay

nextPositionMonthly' :: MonthlyPatternState -> MonthlyPatternState
nextPositionMonthly' p = nextPosition' p nextMonthDay

nextDays' :: PatternState' a -> (PatternState' a -> PatternState' a) -> [Day]
nextDays' p f = map getDay . unfoldr (Just . dupe . f) $ p
  where
    getDay (PatternState' _ p') = _day p'

nextDaysWeekly
