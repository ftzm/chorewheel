{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}

module Chore where

import Data.Generics.Internal.VL.Lens
import Data.Generics.Labels ()
-- instance declarations

import Data.List ((\\))
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Time.Clock
import Data.UUID
import Debug.Trace
import GHC.Records (HasField)
import Servant.API

import Models
import Participants
import Schedule

-------------------------------------------------------------------------------

{- | Represents a period of time. The first date is guaranteed to be before the
 second date.
-}
newtype Period = UTCTime UTCTime

newtype ChoreId = ChoreId {unChoreId :: UUID}
  deriving (Eq, Ord, Show)
  deriving newtype (ToHttpApiData, FromHttpApiData)

data Chore = Chore
  { id' :: ChoreId
  , name :: Text
  , schedule :: ScheduleState
  , lastResolution :: Maybe Resolution
  , participants :: Participants
  }
  deriving (Eq, Show, Generic)

data ChoreEvent
  = -- | Complete a task
    Complete UTCTime
  | -- | Skip a task. Has the same effect as completing the task in that the
    -- task is subsequently scheduled for the next logical day, but makes the
    -- distinction that the task was not actually completed.
    Skip UTCTime

data ChoreStatus
  = -- | the chore is scheduled some time in the future.
    NotDue
  | -- | the chore is scheduled for today
    Due
  | -- | the chore was scheduled for before today, and between that day and
    -- today the chore should have been scheduled n additional times.
    Overdue Int

data ChoreNameExists = ChoreNameExists
  deriving (Show)
instance Exception ChoreNameExists

-- where resolutionDay is later than the previous scheduled, return the lapsed
-- days where they exist, and the resolution fed in, in a list.
doChore ::
  Chore ->
  Resolution ->
  Either ResolutionError ([Resolution], Chore)
doChore c@Chore{..} resolution =
  resolveSchedule schedule ((.day) <$> lastResolution) resolution
    <&> \(resolutions, nextScheduleState) ->
      ( resolutions
      , c
          & #schedule .~ nextScheduleState
          & #lastResolution .~ (Just resolution)
      )

enrichById :: (Foldable t, Eq b, HasField "id'" a b) => t a -> [b] -> [a]
enrichById s = catMaybes . map (\i -> find ((i ==) . (.id')) s)

genRotation :: [Resolution] -> HouseholdMembers -> Participants -> [User]
genRotation resolutions (HouseholdMembers hm) participants =
  cycle $ enrichById hm $ Debug.Trace.trace (show order) order
 where
  sortedResolutions = sortBy (\x y -> compare x.day y.day) resolutions
  participantSet = case participants of
    Some ps -> NESet.toSet ps
    Everyone -> Set.map (.id') $ NESet.toSet hm
    None -> mempty
  completions =
    ordNub
      [ u | Completed u <- map resolutionType sortedResolutions, Set.member u participantSet
      ]
  order = Debug.Trace.trace (show completions) (toList participantSet \\ completions) ++ completions
