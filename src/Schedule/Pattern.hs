module Schedule.Pattern
  ( WeeklyPattern
  , MonthlyPattern
  , WeeklyPatternState
  , MonthlyPatternState
  , Pattern (..)
  , PatternState (..)
  , PatternPosition (..)
  , resolvePatternWeekly
  , resolvePatternMonthly
  , nextEligibleDayWeekly
  , nextEligibleDayMonthly
  , futureStatesWeekly
  , futureStatesMonthly
  , PatternStateError (..)
  , createPattern
  , createWeeklyState
  , createMonthlyState
  ) where

import Data.Set.NonEmpty (NESet, size, elemAt)
import Data.Time.Calendar hiding (DayOfMonth)
import Schedule.Primitives
import qualified Data.Set.NonEmpty as NESet
import Control.Monad.Except

data Pattern a = Pattern
  -- ^ set of (iteration index, position in iteration)
  { elems :: NESet (Int, a)
  -- ^ size of iteration (week/month)
  , iterations :: Int
  } deriving (Eq, Show)
type WeeklyPattern = Pattern Weekday
type MonthlyPattern = Pattern DayOfMonth

data PatternState a = PatternState (Pattern a) PatternPosition deriving (Eq, Show)
type WeeklyPatternState = PatternState Weekday
type MonthlyPatternState = PatternState DayOfMonth

data PatternPosition = PatternPosition
  { day :: Day
  , index :: Int
  } deriving (Eq, Show)

data PatternError
  = IterationsNonPositive
  | InvalidElemDay
  | DaysEmpty
  | ElemOutOfBounds

data PatternStateError
  = IndexOutOfBounds
  | DayInvalid

-- mkWeeklyPatternState
--   :: Int
--   -> [(Int, Int)]
--   -> Int
--   -> Day
--   -> Either PatternStateError WeeklyPatternState
-- mkWeeklyPatternState = undefined
--
-- unWeeklyPatternState :: WeeklyPatternState -> (Int,  [(Int, Int)], Int, Day)
-- unWeeklyPatternState = undefined

getDay :: PatternState a -> Day
getDay (PatternState _ pos') = pos'.day

nextWeekDay :: Day -> Int -> (Int, Weekday) -> (Int, Weekday) -> Day
nextWeekDay startDay iterations startElem nextElem =
  addDays (fromIntegral $ weekOffset + dayOffset) startDay
  where
    weekOffset | startElem <= nextElem =  7 * (fst nextElem - fst startElem)
               | otherwise = 7 * (iterations + fst nextElem - fst startElem)
    dayOffset = fromEnum (snd nextElem) - fromEnum (snd startElem)

nextMonthDay :: Day -> Int -> (Int, DayOfMonth) -> (Int, DayOfMonth) -> Day
nextMonthDay startDay iterations startElem nextElem =
  fromGregorian newYear newMonth newDay
  where
    monthOffset | startElem <= nextElem =  fst nextElem - fst startElem
                | otherwise = iterations + fst nextElem - fst startElem
    newDay = unDayOfMonth $ snd nextElem
    (newYear, newMonth, _) =
      toGregorian $ addGregorianMonthsClip (fromIntegral monthOffset) startDay

-------------------------------------------------------------------------------

elemAtEither :: Int -> NESet a -> Either PatternStateError a
elemAtEither index set =
  if index < size set
  then Right $ elemAt index set
  else Left IndexOutOfBounds

createPattern
  :: Ord a
  => (Int -> Maybe a)
  -> [(Int, Int)]
  -> Int
  -> Either PatternError (Pattern a)
createPattern f elems iterations = do
  when (iterations < 1) $ throwError IterationsNonPositive
  when ((any ((>= iterations) . fst)) elems) $ throwError ElemOutOfBounds
  when ((any ((< 0) . fst)) elems) $ throwError ElemOutOfBounds
  enumElems <- maybeToEither InvalidElemDay $ traverse (traverse f) elems
  elemSet <- maybeToEither DaysEmpty $ (NESet.fromList <$> nonEmpty enumElems)
  return $ Pattern elemSet iterations
  where
    maybeToEither e = maybe (Left e) Right

validatePatternState
  :: Eq a
  => (Day -> a)
  -> Pattern a
  -> PatternPosition
  -> Either PatternStateError (PatternState a)
validatePatternState f pat@Pattern{..} pos@PatternPosition {..} =
  bool (Left DayInvalid) (Right $ PatternState pat pos) .
  (f day ==) . snd =<< elemAtEither index elems

createWeeklyState
  :: WeeklyPattern
  -> PatternPosition
  -> Either PatternStateError WeeklyPatternState
createWeeklyState = validatePatternState getWeekday

createMonthlyState
  :: MonthlyPattern
  -> PatternPosition
  -> Either PatternStateError MonthlyPatternState
createMonthlyState = validatePatternState getDayOfMonth

nextPosition
  :: (Day -> Int -> (Int, a) -> (Int, a) -> Day)
  -> PatternState a
  -> PatternState a
nextPosition f (PatternState pat@Pattern{..} (PatternPosition {..})) =
  PatternState pat $ PatternPosition nextDay nextIndex
  where
    nextIndex = if index == size elems - 1 then 0 else index + 1
    nextDay = f day iterations (elemAt index elems) (elemAt nextIndex elems)

nextPositionWeekly :: WeeklyPatternState -> WeeklyPatternState
nextPositionWeekly = nextPosition nextWeekDay

nextPositionMonthly :: MonthlyPatternState -> MonthlyPatternState
nextPositionMonthly = nextPosition nextMonthDay

nextDays :: (PatternState a -> PatternState a) -> PatternState a -> [PatternState a]
nextDays f = unfoldr (Just . dupe . f)

futureStatesWeekly :: WeeklyPatternState -> [WeeklyPatternState]
futureStatesWeekly = nextDays nextPositionWeekly

futureStatesMonthly :: MonthlyPatternState -> [MonthlyPatternState]
futureStatesMonthly = nextDays nextPositionMonthly

nextEligibleDay
  :: Eq a
  => (Day -> a) -- ^ The function to make days pattern-comparable
  -> Pattern a -- ^ The pattern
  -> Int -- ^ Current index in the pattern
  -> Day -- ^ Day to start searching from
  -> Either PatternStateError (PatternState a)
nextEligibleDay f p@Pattern{elems} i d =
  constr . findDay <$> elemAtEither i elems
  where
    constr = PatternState p . flip PatternPosition i
    findDay (_,a) = (viaNonEmpty head $ filter ((a==) . f) [d..]) & \case
      Just x -> x
      Nothing -> error "Impossible: no eligible next day found."

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

resolvePatternWeekly
  :: WeeklyPatternState
  -> Day
  -> ([Day], Maybe WeeklyPatternState)
resolvePatternWeekly p@(PatternState _ pos) day
  | pos.day > day = ([], Nothing)
  | otherwise = bimap (map getDay) (viaNonEmpty head)
                $ span ((day>) . getDay) $ p : futureStatesWeekly p

resolvePatternMonthly
  :: MonthlyPatternState
  -> Day
  -> ([Day], Maybe MonthlyPatternState)
resolvePatternMonthly p@(PatternState _ pos) day
  | pos.day > day = ([], Nothing)
  | otherwise = bimap (map getDay) (viaNonEmpty head)
                $ span ((day>) . getDay) $ p : futureStatesMonthly p
