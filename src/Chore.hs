module Chore where

import Data.Time.Clock
import Data.Time.Calendar
--import Data.Time.Calendar.OrdinalDate
import Data.Set (Set(..))

--data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Eq

--data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

data IntervalUnit = Day | Week | Month | Year

data Schedule = Repeat Int IntervalUnit | Weekly (Set DayOfWeek)

-- | Represents a period of time. The first date is guaranteed to be before the
-- second date.
data Period = UTCTime UTCTime

data Resolution
  = Completed UTCTime
  | Cancelled UTCTime
  | Rescheduled UTCTime

data ChoreEvent
  -- | Complete a task
  = Complete UTCTime
  -- | Cancel a task. Has the same effect as completing the task in that the
  -- task is subsequently scheduled for the next logical day, but makes the
  -- distinction that the task was not actually completed.
  | Cancel UTCTime
  -- | Apply a pause period to a task. This has the effect of moving an
  -- existing or newly scheduled task to after the pause period has ended.
  | Pause Period

data ChoreStatus
  -- | the chore is scheduled for today
  = Due
  -- | the chore is scheduled for before today, but the next logical
  -- occurrence is after today
  | Overdue
  -- | The chore was not done on its scheduled day, and between that day and
  -- today the chore should have been scheduled n additional times.
  | Skipped Int

data ChoreState = ChoreState
  -- | the first scheduled occurrence of this task. This represents either the
  -- first scheduled instance of a task ever, or the first scheduled instance
  -- of a task since after if was last completed.
  { firstScheduled :: Day
  , scheduled :: Schedule
  }

scheduleStep :: Schedule -> Day -> Day
scheduleStep (Weekly days) d =
  head $ filter ((`elem` days) . dayOfWeek) [(succ d)..]
scheduleStep (Repeat interval unit) d = case unit of
  Day -> undefined
  Month -> undefined
  Year -> undefined

data ChoreStatus' = ChoreStatus'

-- resolve

-- Given a ChoreState and a date, produce the status for that day; be it
-- overdue, gone around, what have you.
