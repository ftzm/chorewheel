module Schedule.Primitives where

import Data.Time.Calendar hiding (DayOfMonth)

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq, Ord, Bounded, Enum)

getWeekday :: Day -> Weekday
getWeekday = toEnum . subtract 1 . fromEnum . dayOfWeek

safeToEnum :: forall t. (Enum t, Bounded t) => Int -> Maybe t
safeToEnum i
  | (i >= fromEnum (minBound :: t)) &&
    (i <= fromEnum (maxBound :: t)) = Just . toEnum $ i
  | otherwise = Nothing

mkWeekday :: Int -> Maybe Weekday
mkWeekday = safeToEnum

newtype DayOfMonth = DayOfMonth {unDayOfMonth :: Int} deriving (Eq, Ord, Show)

mkDayOfMonth :: Int -> Maybe DayOfMonth
mkDayOfMonth i | i > 31 = Nothing
               | otherwise = Just $ DayOfMonth i

getDayOfMonth :: Day -> DayOfMonth
getDayOfMonth = DayOfMonth . (\(_, _, d) -> d ) . toGregorian

--Mo Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Show, Eq, Ord, Enum)

-- zero indexed, unlike the time library
getMonthInt :: Day -> Int
getMonthInt = subtract 1 . (\(_, m, _) -> m ) . toGregorian

dupe :: a -> (a, a)
dupe x = (x, x)
