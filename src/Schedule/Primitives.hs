{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Schedule.Primitives where

import Data.Time.Calendar

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq, Ord, Bounded, Enum)

getWeekday :: Day -> Weekday
getWeekday = toEnum . subtract 1 . fromEnum . dayOfWeek


-- >>> toEnum . fromEnum $ Wednesday :: Weekday
-- Thu

-- >>> getWeekday wed
-- Wed

-- >>> toEnum 1 :: Weekday
-- Tue

safeToEnum :: forall t. (Enum t, Bounded t) => Int -> Maybe t
safeToEnum i
  | (i >= fromEnum (minBound :: t)) &&
    (i <= fromEnum (maxBound :: t)) = Just . toEnum $ i
  | otherwise = Nothing

mkWeekday :: Int -> Maybe Weekday
mkWeekday = safeToEnum

newtype DayOfMonth = DayOfMonth Int

mkDayOfMonth :: Int -> Maybe DayOfMonth
mkDayOfMonth i | i > 31 = Nothing
               | otherwise = Just $ DayOfMonth i

getDayOfMonth :: Day -> DayOfMonth
getDayOfMonth = DayOfMonth . (\(_, _, d) -> d ) . toGregorian

--Mo Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Show, Eq, Ord, Enum)

-- zero indexed, unlike the time library
getMonthInt :: Day -> Int
getMonthInt = subtract 1 . (\(_, m, _) -> m ) . toGregorian

-- getMonth :: Day -> Month
-- getMonth = toEnum . getMonthEnumInt

--data YearDate = Date Month DayOfMonth

class FromDay a where
  fromDay :: Day -> a

instance FromDay Weekday where
  fromDay = getWeekday

instance FromDay DayOfMonth where
  fromDay = getDayOfMonth
