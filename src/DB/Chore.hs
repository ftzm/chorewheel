{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE LambdaCase #-}

module DB.Chore where

import Hasql.Statement (Statement(..))
import Hasql.TH
import Data.Profunctor (dimap)
import Data.Bifunctor (bimap)
import Data.Vector

import Models
import Chore

insertChore :: Statement (HouseholdId, Chore) ChoreId
insertChore =
  dimap (bimap unHouseholdId _name) ChoreId
  [singletonStatement|
    insert into chore (household_id, name)
    values ($1 :: int4, $2 :: text)
    returning id :: int4 |]

householdChores :: Statement HouseholdId (Vector (ChoreId, Chore))
householdChores =
  dimap unHouseholdId (fmap $ bimap ChoreId Chore)
  [vectorStatement|
    select c.id :: int4, c.name :: text
    from chore c
    where c.household_id = $1 :: int4
    order by name|]
