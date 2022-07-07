{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards          #-}

module DB.Household where

import Data.Bifunctor (bimap)
import Data.Profunctor (dimap, lmap)
import Hasql.Statement (Statement(..))
import Hasql.TH
import Models (Household(..), HouseholdId(..), UserId(..))
import Data.Vector

insertHousehold :: Statement Household HouseholdId
insertHousehold =
  dimap unHousehold HouseholdId
  [singletonStatement|
    insert into household (name)
    values ($1 :: text)
    returning id :: int4|]

insertHouseholdMember :: Statement (HouseholdId, UserId) ()
insertHouseholdMember =
  lmap (bimap unHouseholdId unUserId)
  [resultlessStatement|
    insert into household_member (household_id, user_id)
    values ($1 :: int4, $2 :: int4)|]

getUserHouseholds :: Statement UserId (Vector (HouseholdId, Household))
getUserHouseholds =
  dimap unUserId (fmap $ bimap HouseholdId Household)
  [vectorStatement|
    select h.id :: int4, h.name :: text
    from household h
    join household_member hm on hm.household_id = h.id
    where hm.user_id = $1 :: int4 |]