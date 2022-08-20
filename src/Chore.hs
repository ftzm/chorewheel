{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Chore where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Text
import Data.UUID

-------------------------------------------------------------------------------
-- | Represents a period of time. The first date is guaranteed to be before the
-- second date.
newtype Period = UTCTime UTCTime

data Resolution
  = Completed UTCTime
  | Cancelled UTCTime
  | Rescheduled UTCTime

newtype ChoreId = ChoreId { unChoreId :: UUID} deriving (Eq, Show)

data Chore = Chore
  { id' :: ChoreId
  , name :: Text
  } deriving (Eq, Show)

data ChoreEvent
  -- | Complete a task
  = Complete UTCTime
  -- | Skip a task. Has the same effect as completing the task in that the
  -- task is subsequently scheduled for the next logical day, but makes the
  -- distinction that the task was not actually completed.
  | Skip UTCTime

data ChoreStatus
  -- | the chore is scheduled some time in the future.
  = NotDue
  -- | the chore is scheduled for today
  | Due
  -- | the chore was scheduled for before today, and between that day and
  -- today the chore should have been scheduled n additional times.
  | Overdue Int

newtype Scheduled = Scheduled { unScheduled :: Day}
newtype Resolved = Resolved { unResolved :: Day}
