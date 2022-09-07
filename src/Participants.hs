module Participants where

import Models
import qualified Data.Set.NonEmpty as NESet

data Participants
  -- select from all users the user who has last (or never, and then the oldest
  -- user) done the task.
  = Everyone
  -- Selecting from a limited set of household members.
  | None
  -- None
  | Some (NESet.NESet UserId)
  deriving (Eq, Show, Generic)
