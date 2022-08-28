{-# LANGUAGE QuasiQuotes          #-}

module DB.Chore where

import Hasql.Statement (Statement(..))
import Hasql.TH
import Data.Profunctor (lmap, dimap)
import qualified Data.Vector as V
import Text.RawString.QQ
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Data.Tuple.Sequence

import Models
import Chore
import Schedule
import Schedule.Pattern
import Schedule.Primitives
import DB.Util

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

-- TODO: actually load last resolution date
getFullChoresByHousehold :: Statement HouseholdId (V.Vector Chore)
getFullChoresByHousehold = Statement sql encoder (D.rowVector rowDecoder) True
  where
    encoder = unHouseholdId >$< E.param (E.nonNullable E.uuid)
    rowDecoder = rawResult <&> \raw@((choreId, choreName), _, _, _, _, _) ->
      (\s -> Chore (ChoreId choreId) choreName s Nothing) $ case raw of
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
      join schedule s on s.chore_id = c.id
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
