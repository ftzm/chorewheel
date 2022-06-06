{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Schedule.Pattern where

import Data.Set (Set, elemAt, size)
import Data.Time.Calendar
import Data.List (unfoldr)
import Schedule.Primitives

-- table for pattern without position
-- table for nextsheduled with day and position

data PatternPeriod = Weekly | Monthly deriving Show

data PatternError
  = IndexOutOfRange
  | PositionInvalid

data Pattern = Pattern
  -- ^ set of (iteration index, position in iteration)
  { _elems :: Set (Int, Int)
  -- ^ size of iteration (week/month)
  , _period :: PatternPeriod
  -- ^ number of weeks/months in the pattern
  , _iterations :: Int
  } deriving Show

data PatternPosition = PatternPosition
  { _day :: Day
  , _index :: Int
  } deriving Show

elemAtSafe :: Int -> Set a -> Maybe a
elemAtSafe index set =
  if index < size set
  then Just $ elemAt index set
  else Nothing

nextPositionSafe :: Pattern -> PatternPosition -> Maybe PatternPosition
nextPositionSafe pat pos
  | _index pos < size (_elems pat) = Just $ nextPosition pat pos
  | not $ validatePosition pat pos = Nothing
  | otherwise = Nothing

validatePosition :: Pattern -> PatternPosition -> Bool
validatePosition Pattern {..} PatternPosition {..} =
  case _period of
    Weekly -> ((getWeekday _day ==) <$> mkWeekday (snd posElem)) == Just True
    Monthly -> getMonthInt _day == snd posElem
  where
    posElem = elemAt _index _elems --TODO: unsafe!

nextPosition :: Pattern -> PatternPosition -> PatternPosition
nextPosition Pattern {..} PatternPosition {..} =
    let
      elemCount = size _elems
      startElem = elemAt _index _elems
      nextIndex = if _index == elemCount - 1 then 0 else _index + 1
      nextElem = elemAt nextIndex _elems
      nextDay =
        case _period of
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
     in PatternPosition nextDay nextIndex

dupe :: a -> (a, a)
dupe x = (x, x)

nextDays :: Pattern -> PatternPosition -> [PatternPosition]
nextDays pat = unfoldr (Just . dupe . nextPosition pat)

mkPattern :: [(Int, Int)] -> PatternPeriod -> Int -> Maybe Pattern
mkPattern = undefined
