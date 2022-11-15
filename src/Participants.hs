module Participants where

import Data.Set.NonEmpty qualified as NESet
import Models

data Participants
  = -- select from all users the user who has last (or never, and then the oldest
    -- user) done the task.
    Everyone
  | -- Selecting from a limited set of household members.
    None
  | -- None
    Some (NESet.NESet UserId)
  deriving (Eq, Show, Generic)
