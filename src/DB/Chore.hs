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
    rowDecoder =
      rawResult <&> \(choreId, choreName, p, flex, strict, weekly, monthly) ->
      let s = fromMaybe UnscheduledSS $ flex <|> strict <|> weekly <|> monthly
      in Chore (ChoreId choreId) choreName s Nothing p
    (<$$>) = fmap . fmap
    rawResult = (,,,,,,)
      <$> col D.uuid
      <*> col D.text
      <*> col participants
      <*> (parseFlex <$$> simple)
      <*> (parseStrict <$$> simple)
      <*> (parseWeekly <$$> pat)
      <*> (parseMonthly <$$> pat)
    col = D.column . D.nonNullable
    fieldN = D.field . D.nullable
    arr = D.listArray . D.nonNullable
    simple = sequenceT <$> col
      (D.composite $ (,) <$> fieldN D.int4 <*> fieldN D.date)
    pat = sequenceT <$> col
      ( D.composite $ (,,,,)
      <$> fieldN D.int4
      <*> (fieldN . arr) D.int4
      <*> (fieldN . arr) D.int4
      <*> fieldN D.int4
      <*> fieldN D.date
      )
    participants = D.composite $ toParticipants'
      <$> (D.field . D.nonNullable) D.text
      <*> (V.fromList <$> (D.field . D.nonNullable . arr) D.uuid)
    parseFlex (days, scheduled) =
      FlexDaysSS $ FlexDaysState (FlexDays $ fromIntegral days) scheduled
    parseStrict (days, scheduled) =
      StrictDaysSS $ StrictDaysState (StrictDays $ fromIntegral days) scheduled
    parseWeekly (i, ei, ep, index, scheduled) =
      let elemDays = map (toEnum . fromIntegral) ep
          elems = loadNESetUnsafe $ zip (map fromIntegral ei) elemDays
      in WeeklyPatternSS $ PatternState (Pattern elems $ fromIntegral i)
         $ PatternPosition scheduled $ fromIntegral index
    parseMonthly (i, ei, ep, index, scheduled) =
      let elemDays = map (DayOfMonth . fromIntegral)  ep
          elems = loadNESetUnsafe $ zip (map fromIntegral ei) elemDays
      in MonthlyPatternSS $ PatternState (Pattern elems $ fromIntegral i)
         $ PatternPosition scheduled $ fromIntegral index
    sql = [r|
      select
        c.id,
        c.name,
        (p.type, p.participants),
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
      join (
        select
          cpt.chore_id,
          cpt.type,
          array_remove(array_agg(cp.user_id), null) participants
        from chore_participant_type cpt
        left join chore_participant cp on cp.chore_id = cpt.chore_id
        group by cpt.chore_id, cpt.type
      ) p on p.chore_id = c.id
      where c.household_id = $1 :: uuid
      order by c.name
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

toParticipants :: (Text, V.Vector UUID) -> Participants
toParticipants ("everyone", _) = Everyone
toParticipants ("none", _) = None
toParticipants ("some", ids) = Some $ loadNESetUnsafeV $ V.map UserId ids
toParticipants _ = error "invalid participants constructor"

toParticipants' :: Text -> V.Vector UUID -> Participants
toParticipants' "everyone" _ = Everyone
toParticipants' "none" _ = None
toParticipants' "some" ids = Some $ loadNESetUnsafeV $ V.map UserId ids
toParticipants' _ _ = error "invalid participants constructor"

getChoreParticipants :: Statement ChoreId Participants
getChoreParticipants =
  dimap unChoreId toParticipants
  [singletonStatement|
  select cpt.type :: text, array_remove(array_agg(cp.user_id), null) :: uuid[]
  from chore_participant_type cpt
  left join chore_participant cp on cp.chore_id = cpt.chore_id
  where cpt.chore_id = $1 :: uuid
  group by cpt.chore_id, cpt.type
  |]
