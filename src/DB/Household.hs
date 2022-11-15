{-# LANGUAGE QuasiQuotes #-}

module DB.Household where

import DB.Util
import Data.Profunctor (dimap, lmap)
import Data.Tuple.Sequence
import Data.UUID (UUID)
import Data.Vector qualified as V
import Hasql.Statement (Statement (..))
import Hasql.TH
import Models (
  Household (..),
  HouseholdId (..),
  HouseholdMembers (..),
  User (..),
  UserId (..),
 )

insertHousehold :: Statement (HouseholdId, Text) ()
insertHousehold =
  lmap
    (\(HouseholdId id', name) -> (id', name))
    [resultlessStatement|
    insert into household (id, name)
    values ($1 :: uuid, $2 :: text)|]

insertHouseholdMember :: Statement (HouseholdId, UserId) ()
insertHouseholdMember =
  lmap
    (bimap unHouseholdId unUserId)
    [resultlessStatement|
    insert into household_member (household_id, user_id)
    values ($1 :: uuid, $2 :: uuid)|]

getUserHouseholds :: Statement UserId (V.Vector Household)
getUserHouseholds =
  dimap
    unUserId
    decoder
    [vectorStatement|
  select
    id :: uuid,
    name :: text,
    user_ids :: uuid[],
    names :: text[],
    emails :: text[]
  from (select
    h.id,
    h.name,
    array_agg(u.id) as user_ids,
    array_agg(u.name) as names,
    array_agg(u.email) as emails
  from household h
  join household_member hm on h.id = hm.household_id
  join "user" u on hm.user_id = u.id
  group by h.id, h.name) as households
  where ($1 :: uuid) = any (households.user_ids)
  |]
 where
  decoder ::
    V.Vector (UUID, Text, V.Vector UUID, V.Vector Text, V.Vector Text) ->
    V.Vector Household
  decoder = V.map $ \(i, name, userIds, names, emails) ->
    let members = loadMembers $ V.zip3 userIds names emails
     in Household (HouseholdId i) name members
  loadMembers =
    HouseholdMembers
      . loadNESetUnsafeV
      . V.map (\(i, n, e) -> User (UserId i) n e)

isHouseholdMember :: Statement (UserId, HouseholdId) Bool
isHouseholdMember =
  lmap
    (bimap unUserId unHouseholdId)
    [singletonStatement|
  select exists (
    select * from household_member
    where user_id = $1 :: uuid
    and household_id = $2 :: uuid
  ) :: bool|]

removeHouseholdMember :: Statement (HouseholdId, UserId) ()
removeHouseholdMember =
  lmap
    (bimap unHouseholdId unUserId)
    [resultlessStatement|
    delete from household_member
    where household_id = $1 :: uuid
    and user_id = $2 :: uuid|]

deleteEmptyHousehold :: Statement HouseholdId ()
deleteEmptyHousehold =
  lmap
    unHouseholdId
    [resultlessStatement|
    delete from household h
    where h.id = $1 :: uuid
    and not exists (
      select null from household_member where household_id = h.id
    )|]

getHouseholdIdFromName :: Statement (UserId, Text) (Maybe HouseholdId)
getHouseholdIdFromName =
  dimap
    (first unUserId)
    (fmap HouseholdId)
    [singletonStatement|
    select h.id :: uuid?
    from household h
    join household_member hm on hm.household_id = h.id
    where hm.user_id = $1 :: uuid
    and h.name = $2 :: text|]

getHouseholdMembers :: Statement HouseholdId HouseholdMembers
getHouseholdMembers =
  dimap
    unHouseholdId
    loadMembers
    [vectorStatement|
  select u.id :: uuid, u.name :: text, u.email :: text
  from household_member hm
  join "user" u on hm.user_id = u.id
  where hm.household_id = $1 :: uuid|]
 where
  loadMembers =
    HouseholdMembers
      . loadNESetUnsafeV
      . V.map (\(i, n, e) -> User (UserId i) n e)

getHouseholdByName :: Statement (UserId, Text) (Maybe Household)
getHouseholdByName =
  dimap
    (\(UserId i, name) -> (i, name))
    (fmap decoder . sequenceT)
    [singletonStatement|
  select
    id :: uuid?,
    name :: text?,
    user_ids :: uuid[]?,
    names :: text[]?,
    emails :: text[]?
  from (select
    h.id,
    h.name,
    array_agg(u.id) as user_ids,
    array_agg(u.name) as names,
    array_agg(u.email) as emails
  from household h
  join household_member hm on h.id = hm.household_id
  join "user" u on hm.user_id = u.id
  group by h.id, h.name) as households
  where ($1 :: uuid) = any (households.user_ids)
  and households.name = $2 :: text
  limit 1
  |]
 where
  decoder ::
    (UUID, Text, V.Vector UUID, V.Vector Text, V.Vector Text) ->
    Household
  decoder (i, name, userIds, names, emails) =
    let members = loadMembers $ V.zip3 userIds names emails
     in Household (HouseholdId i) name members
  loadMembers =
    HouseholdMembers
      . loadNESetUnsafeV
      . V.map (\(i, n, e) -> User (UserId i) n e)

getHouseholdById :: Statement (UserId, HouseholdId) (Maybe Household)
getHouseholdById =
  dimap
    (\(UserId i, HouseholdId hId) -> (i, hId))
    (fmap decoder . sequenceT)
    [singletonStatement|
  select
    id :: uuid?,
    name :: text?,
    user_ids :: uuid[]?,
    names :: text[]?,
    emails :: text[]?
  from (select
    h.id,
    h.name,
    array_agg(u.id) as user_ids,
    array_agg(u.name) as names,
    array_agg(u.email) as emails
  from household h
  join household_member hm on h.id = hm.household_id
  join "user" u on hm.user_id = u.id
  group by h.id, h.name) as households
  where ($1 :: uuid) = any (households.user_ids)
  and households.id = $2 :: uuid
  limit 1
  |]
 where
  decoder ::
    (UUID, Text, V.Vector UUID, V.Vector Text, V.Vector Text) ->
    Household
  decoder (i, name, userIds, names, emails) =
    let members = loadMembers $ V.zip3 userIds names emails
     in Household (HouseholdId i) name members
  loadMembers =
    HouseholdMembers
      . loadNESetUnsafeV
      . V.map (\(i, n, e) -> User (UserId i) n e)
