{-# LANGUAGE QuasiQuotes          #-}

module DB.Chore where

import Hasql.Statement (Statement(..))
import Hasql.TH
import Data.Profunctor (lmap, dimap)
import qualified Data.Vector as V
import Text.RawString.QQ
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import qualified Hasql.Session as Session
import Data.Tuple.Sequence
import Data.Time.Calendar (Day)

import Models
import Chore
import Schedule
import Participants
import Schedule.Pattern
import Schedule.Primitives
import DB.Util
import Data.UUID (UUID)

insertChore :: Statement (HouseholdId, ChoreId, Text) ()
insertChore =
  lmap (\(HouseholdId hi, ChoreId id', name) -> (id', hi, name))
  [resultlessStatement|
  insert into chore (id, household_id, name)
  values ($1 :: uuid, $2 :: uuid, $3 :: text)|]

householdChores :: Statement HouseholdId (V.Vector (ChoreId, Text))
householdChores =
  dimap unHouseholdId (fmap (\(i, n) -> (ChoreId i, n)))
  [vectorStatement|
  select c.id :: uuid, c.name :: text
  from chore c
  where c.household_id = $1 :: uuid
  order by name|]

-- TODO: actually load participants
getFullChoresByHousehold :: Statement HouseholdId (V.Vector Chore)
getFullChoresByHousehold = Statement sql encoder (D.rowVector rowDecoder) True
  where
    encoder = unHouseholdId >$< E.param (E.nonNullable E.uuid)
    rowDecoder = rawResult <&> \raw@((choreId, choreName), _, _, _, _, _) ->
      (\s -> Chore (ChoreId choreId) choreName s Nothing Everyone) $ case raw of
      (_, "flex_days", Just (days, scheduled), _, _, _) ->
        FlexDaysSS $ FlexDaysState (FlexDays $ fromIntegral days) scheduled
      (_, "strict_days", _, Just (days, scheduled), _, _) ->
        StrictDaysSS $ StrictDaysState (StrictDays $ fromIntegral days) scheduled
      (_, "weekly_pattern", _, _, Just (i, ei, ep, index, scheduled), _) ->
        let elemDays = map (toEnum . fromIntegral) ep
            elems = loadNESetUnsafe $ zip (map fromIntegral ei) elemDays
        in WeeklyPatternSS $ PatternState (Pattern elems $ fromIntegral i)
           $ PatternPosition scheduled $ fromIntegral index
      (_, "monthly_pattern", _, _, _, Just (i, ei, ep, index, scheduled)) ->
        let elemDays = map (DayOfMonth . fromIntegral)  ep
            elems = loadNESetUnsafe $ zip (map fromIntegral ei) elemDays
        in MonthlyPatternSS $ PatternState (Pattern elems $ fromIntegral i)
           $ PatternPosition scheduled $ fromIntegral index
      e -> error $ "impossible due to DB constraints: " <> show e
      where
        rawResult = (,,,,,)
          <$> colM chore
          <*> colM D.text
          <*> (sequenceT <$> colM simple)
          <*> (sequenceT <$> colM simple)
          <*> (sequenceT <$> colM pat)
          <*> (sequenceT <$> colM pat)
        colM = D.column . D.nonNullable
        fieldM = D.field . D.nullable
        arr = D.array . D.dimension replicateM . D.element . D.nonNullable
        chore = D.composite $ (,)
          <$> (D.field . D.nonNullable) D.uuid
          <*> (D.field . D.nonNullable) D.text
        simple = D.composite $ (,) <$> fieldM D.int4 <*> fieldM D.date
        pat = D.composite $ (,,,,)
          <$> fieldM D.int4
          <*> (fieldM . arr) D.int4
          <*> (fieldM . arr) D.int4
          <*> fieldM D.int4
          <*> fieldM D.date
    sql = [r|
      select
        (c.id, c.name),
        s.type,
        (fd.days, fd.scheduled),
        (sd.days, sd.scheduled),
        (wp.iterations, wp.elem_iterations, wp.elem_points, wp.elem_index, wp.scheduled),
        (mp.iterations, mp.elem_iterations, mp.elem_points, mp.elem_index, mp.scheduled)
      from chore c
      join schedule s on s.id = c.id
      left join flex_days fd on fd.id = s.id
      left join strict_days sd on sd.id = s.id
      left join (
        select
          wp.id,
          wp.iterations,
          wp.elem_index,
          wp.scheduled,
          array_agg(iteration) elem_iterations,
          array_agg(point) elem_points
        from weekly_pattern wp
        join weekly_pattern_elem wpe on wp.id = wp.id
        group by wp.id, wpe.weekly_pattern_id
      ) wp on wp.id = s.id
      left join (
        select
          mp.id,
          mp.iterations,
          mp.elem_index,
          mp.scheduled,
          array_agg(iteration) elem_iterations,
          array_agg(point) elem_points
        from monthly_pattern mp
        join monthly_pattern_elem mpe on mp.id = mp.id
        group by mp.id, mpe.monthly_pattern_id
      ) mp on mp.id = s.id
      where c.household_id = $1 :: uuid
      |]

insertChoreEvents
  :: Statement (V.Vector (ChoreId, Resolution)) ()
insertChoreEvents = Statement sql encoder D.noResult True
  where
    sql =
      "insert into chore_event (chore_id, day, type, user_id)\
      \select * from unnest ($1, $2, $3, $4)"
    flatten = contramap $ V.map $ \(c, Resolution {..}) -> case resolutionType of
      Completed userId -> (c, day, "completed", Just userId)
      Lapsed -> (c, day, "lapsed", Nothing)
      Skipped -> (c, day, "skipped", Nothing)
    encoder' = vectorEncoder4N
      (E.nonNullable $ unChoreId >$< E.uuid)
      (E.nonNullable E.date)
      (E.nonNullable E.text)
      (E.nullable $ unUserId >$< E.uuid)
    encoder = flatten encoder'

toResolutionType :: Text -> Maybe UUID -> ResolutionType
toResolutionType "completed" (Just userId) = Completed $ UserId userId
toResolutionType "skpipped" _ = Skipped
toResolutionType "lapsed" _ = Lapsed
toResolutionType _ _ = error "invalid ResolutionType constructor"

getChoreEvents :: Statement ChoreId (V.Vector Resolution)
getChoreEvents =
  dimap unChoreId (V.map (\(d, c, userIdO) -> Resolution d (toResolutionType c userIdO)))
  [vectorStatement|
  select day :: date, type :: text, user_id :: uuid?
  from chore_event
  where chore_id = $1 :: uuid
  order by day asc|]

-- Includes the last resolution before the range to be able to generate a
-- complete history for the range (i.e. to be able to show if the first
-- completion in the range was overdue)
getChoreEventsFrom :: Statement (ChoreId, Day) (V.Vector Resolution)
getChoreEventsFrom =
  dimap (first unChoreId) (V.map (\(d, c, userIdO) -> Resolution d (toResolutionType c userIdO)))
  [vectorStatement|
  select day :: date, type :: text, user_id :: uuid?
  from chore_event
  where chore_id = $1 :: uuid
  and day >= (
    select max(day)
    from chore_event
    where chore_id = $1 :: uuid
    and day < $2 :: date
  )|]

insertParticipants :: (ChoreId, Participants) -> Session.Session ()
insertParticipants args = do
  Session.statement args insertParticipantType
  case args of
    (choreId, Some userIds) ->
      Session.statement (choreId, V.fromList $ toList userIds)
      insertParticipantMembers
    _ -> return ()

insertParticipantType :: Statement (ChoreId, Participants) ()
insertParticipantType =
  lmap
  (bimap unChoreId toName)
  [resultlessStatement|
  insert into chore_participant_type (chore_id, "type")
  values ($1 :: uuid, $2 :: text)|]
  where
    toName = \case
      Everyone -> "everyone"
      None -> "none"
      Some _ -> "some"

insertParticipantMembers :: Statement (ChoreId, V.Vector UserId) ()
insertParticipantMembers = Statement sql encoder D.noResult True
  where
    sql =
      "insert into chore_participant (chore_id, user_id)\
      \select * from unnest ($1, $2)"
    assoc (choreId, userIds) = V.map (choreId,) userIds
    encoder = contramap assoc $ vectorEncoder2
      (unChoreId >$< E.uuid)
      (unUserId >$< E.uuid)

getChoreParticipants :: Statement ChoreId Participants
getChoreParticipants =
  dimap unChoreId toParticipants
  [singletonStatement|
  select cpt.type :: text, array_remove(array_agg(cp.user_id), null) :: uuid[]
  from chore_participant_type cpt
  left join chore_participant cp on cp.chore_id = cpt.chore_id
  where cpt.chore_id = $1 :: uuid
  group by cpt.type
  limit 1|]
  where
    toParticipants ("everyone", _) = Everyone
    toParticipants ("none", _) = None
    toParticipants ("some", ids) = Some $ loadNESetUnsafeV $ V.map UserId ids
    toParticipants _ = error "invalid participants constructor"
