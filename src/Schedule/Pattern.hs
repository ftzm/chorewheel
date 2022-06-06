{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Schedule.Pattern where

import Schedule.Primitives

import Data.Set (Set, fromList, elemAt, size)
import Data.Time.Calendar
import Control.Monad.Except

newtype Iteration = Iteration { unIteration :: Int }
  deriving (Show, Eq, Ord)

data PatternElem a = PatternElem Iteration a
  deriving (Show, Eq, Ord)

newtype PatternElems a = PatternElems {unPatternElems :: Set (PatternElem a)}
  deriving Show

data Pattern a = Pattern
  { _elems :: PatternElems a
  -- -- number of elems in the elems for faster indexing and such
  -- , _elemCount :: Int
  -- the length of a the schedule, aka the number of iterations.
  , _length :: Int
  } deriving Show

mkPattern :: Ord a => PatternElems a -> Int -> Pattern a
mkPattern = Pattern

data CircularCursor = CircularCursor { max' :: Int, position :: Int}
  deriving (Show)

mkCircularCursor :: Int -> Int -> Maybe CircularCursor
mkCircularCursor max' position | position > max' = Nothing
                               | otherwise = Just CircularCursor {..}

mkCircularCursor' :: PatternElems a -> Int -> Maybe CircularCursor
mkCircularCursor' (PatternElems es) = mkCircularCursor (length es)


data PatternPosition = PatternPosition CircularCursor Day
  deriving (Show)

data PatternState a = PatternState (Pattern a) PatternPosition
  deriving (Show)

mkPatternElems :: Ord a => [(Int, a)] -> PatternElems a
mkPatternElems = PatternElems . fromList . map (\(i, x) -> PatternElem (Iteration i) x)

mkPatternState
  :: Ord a
  => FromDay a
  => PatternElems a
  -> Int -- ^ pattern length
  -> Int -- ^ current position in pattern
  -> Day -- ^ current day
  -> Maybe (PatternState a)
mkPatternState es l p d = do
  cursor <- mkCircularCursor' es p
  let (PatternElem _ e) = schedElemByCursor es cursor
  unless (dayPlausible e d) Nothing
  Just $ PatternState (mkPattern es l) $ PatternPosition cursor d

dayPlausible :: forall a. Eq a => FromDay a => a -> Day -> Bool
dayPlausible x d' = x == fromDay @a d'

wed :: Day
wed = ModifiedJulianDay 59661

-- >>> dayPlausible Wed wed
-- True

-- >>> mkPatternState [(1, Mon), (2, Wed)] 3 1 wed
-- <interactive>:19:17-36: error:
--     • Couldn't match expected type ‘PatternElems a’
--                   with actual type ‘[(a0, Weekday)]’
--     • In the first argument of ‘mkPatternState’, namely
--         ‘[(1, Mon), (2, Wed)]’
--       In the expression: mkPatternState [(1, Mon), (2, Wed)] 3 1 wed
--       In an equation for ‘it’:
--           it = mkPatternState [(1, Mon), (2, Wed)] 3 1 wed
--     • Relevant bindings include
--         it :: Maybe (PatternState a) (bound at <interactive>:19:2)

es'' :: PatternElems Weekday
es'' = PatternElems $ fromList $ map (\(i, x) -> PatternElem (Iteration i) x) [(1, Mon), (2, Wed)]

c' :: CircularCursor
c' = CircularCursor 2 2

-- >>> schedElemByCursor es'' c'
-- PatternElem (Iteration {unIteration = 1}) Mon

incCircularCursor :: CircularCursor -> CircularCursor
incCircularCursor c@CircularCursor{ max', position }
  | position == max' = c{position = 0}
  | otherwise = c{ position = position + 1 }

schedElemByCursor :: PatternElems a -> CircularCursor -> PatternElem a
schedElemByCursor (PatternElems s) (CircularCursor{position}) = elemAt position s

-- Maybe instead of storing the schedule with a patternposition, we should store
-- a pattern, the last schedule day, and a function for creating a new
-- PatternState which has stepped forward.

circularOffset :: Int -> Int -> Iteration -> Iteration -> Int
circularOffset l unit (Iteration i1) (Iteration i2)| i1 > i2 = l + i2 - i1
               | otherwise = unit * (i2 - i1)

weekPatternStateStep :: PatternState Weekday -> PatternState Weekday
weekPatternStateStep (PatternState s@(Pattern{_elems, _length}) (PatternPosition cursor day)) =
  PatternState s $ PatternPosition movedCursor nextDay
  where
    movedCursor = incCircularCursor cursor
    currentElem = schedElemByCursor _elems cursor
    nextElem = schedElemByCursor _elems movedCursor
    nextDay = findNextDayOfWeek _length currentElem day nextElem


monthPatternStateStep :: PatternState DayOfMonth -> PatternState DayOfMonth
monthPatternStateStep (PatternState s@(Pattern{_elems, _length}) (PatternPosition cursor day)) =
  PatternState s $ PatternPosition movedCursor nextDay
  where
    movedCursor = incCircularCursor cursor
    currentElem = schedElemByCursor _elems cursor
    nextElem = schedElemByCursor _elems movedCursor
    nextDay = findNextDayOfMonth _length currentElem day nextElem

findNextDayOfWeek :: Int -> PatternElem Weekday -> Day -> PatternElem Weekday -> Day
findNextDayOfWeek l (PatternElem i1 wd1) d (PatternElem i2 wd2) =
  addDays (fromIntegral (weekOffset + weekdayOffset)) d
  where
    weekOffset = circularOffset l 7 i1 i2
    weekdayOffset = fromEnum wd2 - fromEnum wd1

findNextDayOfMonth :: Int -> PatternElem DayOfMonth -> Day -> PatternElem DayOfMonth -> Day
findNextDayOfMonth l (PatternElem i1 _) d (PatternElem i2 (DayOfMonth newDay)) =
  fromGregorian newYear newMonth newDay
  where
    monthOffset = circularOffset l 12 i1 i2
    (newYear, newMonth, _) = toGregorian $ addGregorianMonthsClip (fromIntegral monthOffset) d


-------------------------------------------------------------------------------
-- Simple

-- table for pattern without position
-- table for nextsheduled with day and position

data Period' = Weekly | Monthly deriving Show

data Pattern' = Pattern'
  { _elems' :: Set (Int, Int)
  , _period' :: Period'
  , _iterations :: Int
  } deriving Show

data PatternPosition' = PatternPosition'
  { _day :: Day
  , _index :: Int
  } deriving Show

elemAtSafe :: Int -> Set a -> Maybe a
elemAtSafe index set =
  if index < size set
  then Just $ elemAt index set
  else Nothing


nextPosition :: Pattern' -> PatternPosition' -> Maybe PatternPosition'
nextPosition Pattern' {..} PatternPosition' {..}
  | _index < elemCount =
    let startElem = elemAt _index _elems'
        nextIndex = if _index == elemCount - 1 then 0 else _index + 1
        nextElem = elemAt nextIndex _elems'
        nextDay =
          case _period' of
            Weekly ->
              let weekOffset = 7 * (fst nextElem - fst startElem)
                  dayOffset = snd nextElem - snd startElem
                  offset = weekOffset + dayOffset
               in addDays (fromIntegral offset) _day
            Monthly ->
              let monthOffset = fst nextElem - fst startElem
                  newDay = snd nextElem
                  (newYear, newMonth, _) =
                    toGregorian $ addGregorianMonthsClip (fromIntegral monthOffset) _day
              in fromGregorian newYear newMonth newDay
     in Just $ PatternPosition' nextDay nextIndex
  | otherwise = Nothing
  where
    elemCount = size _elems'
