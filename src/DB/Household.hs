{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards          #-}

module DB.Household where

import Data.Bifunctor (bimap, first)
import Data.Profunctor (dimap, lmap)
import Hasql.Statement (Statement(..))
import Hasql.TH
import Models (Household(..), HouseholdId(..), UserId(..))
import Data.Vector
import Data.Text (Text)

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
  dimap unUserId (fmap (bimap HouseholdId Household))
  [vectorStatement|
    select h.id :: int4, h.name :: text
    from household h
    join household_member hm on hm.household_id = h.id
    where hm.user_id = $1 :: int4 |]

removeHouseholdMember :: Statement (HouseholdId, UserId) ()
removeHouseholdMember =
  lmap (bimap unHouseholdId unUserId)
  [resultlessStatement|
    delete from household_member
    where household_id = $1 :: int4
    and user_id = $2 :: int4|]

deleteEmptyHousehold :: Statement HouseholdId ()
deleteEmptyHousehold =
  lmap unHouseholdId
  [resultlessStatement|
    delete from household h
    where h.id = $1 :: int4
    and not exists (
      select null from household_member where household_id = h.id
    )|]

getHouseholdIdFromName :: Statement (UserId, Text) (Maybe HouseholdId)
getHouseholdIdFromName =
  dimap (first unUserId) (fmap HouseholdId)
  [singletonStatement|
    select h.id :: int4?
    from household h
    join household_member hm on hm.household_id = h.id
    where hm.user_id = $1 :: int4
    and h.name = $2 :: text|]
