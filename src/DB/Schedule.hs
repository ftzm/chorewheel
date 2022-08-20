{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

module DB.Schedule where

--import Data.Time.Calendar (Day)

import Chore
import Contravariant.Extras.Contrazip
import Data.Bifunctor (bimap)
import Data.Functor.Contravariant
import Data.Int (Int32)
import Data.Profunctor (dimap, lmap)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Statement (Statement (..))
import Hasql.TH
import Schedule
import Schedule.Pattern
import Schedule.Primitives


insertConstructor :: Statement (ChoreId, ScheduleState) ScheduleId
insertConstructor =
  dimap
    (bimap unChoreId toName)
    ScheduleId
    [singletonStatement|
    insert into schedule (chore_id, "type")
    values ($1 :: uuid, $2 :: text)
    returning id :: uuid |]
  where
    toName = \case
      FlexDaysSS _ -> "flex_days"
      StrictDaysSS _ -> "strict_days"
      WeeklyPatternSS _ -> "weekly_pattern"
      MonthlyPatternSS _ -> "monthly_pattern"

insertFlexDays :: Statement (ScheduleId, FlexDaysState) ()
insertFlexDays =
  lmap
    (\(ScheduleId i, FlexDaysState (FlexDays n) d) -> (i, fromIntegral n, d))
    [resultlessStatement|
    insert into flex_days (id, days, scheduled)
    values ($1 :: uuid, $2 :: int4, $3 :: date) |]

insertStrictDays :: Statement (ScheduleId, StrictDaysState) ()
insertStrictDays =
  lmap
    (\(ScheduleId i, StrictDaysState (StrictDays n) d) -> (i, fromIntegral n, d))
    [resultlessStatement|
    insert into strict_days (id, days, scheduled)
    values ($1 :: uuid, $2 :: int4, $3 :: date) |]

insertWeeklyPatternMain :: Statement (ScheduleId, WeeklyPatternState) ()
insertWeeklyPatternMain =
  lmap
    (\( ScheduleId i
      , PatternState (Pattern {_iterations}) (PatternPosition {_day, _index})
      ) -> (i, fromIntegral _iterations, fromIntegral _index, _day))
    [resultlessStatement|
    insert into weekly_pattern (id, iterations, elem_index, scheduled)
    values ($1 :: uuid, $2 :: int4, $3 :: int4, $4 :: date) |]

insertWeeklyPatternElems :: Statement (V.Vector (ScheduleId, Int, Weekday)) ()
insertWeeklyPatternElems = Statement sql encoder Decoders.noResult True
  where
    sql =
      "insert into weekly_pattern_elem (weekly_pattern_id, iteration, point)\
      \select * from unnest ($1, $2, $3)"
    vector =
      Encoders.param
        . Encoders.nonNullable
        . Encoders.array
        . Encoders.dimension V.foldl'
        . Encoders.element
        . Encoders.nonNullable
    encoder = contramap V.unzip3 $ contrazip3
      (vector $ unScheduleId >$< Encoders.uuid)
      (vector $ fromIntegral >$< Encoders.int4)
      (vector $ fromIntegral . fromEnum >$< Encoders.int4)

insertWeeklyPattern :: (ScheduleId, WeeklyPatternState) -> Session.Session ()
insertWeeklyPattern args@(s, PatternState Pattern {_elems} _) = do
  Session.statement args insertWeeklyPatternMain
  flip Session.statement insertWeeklyPatternElems
    $ V.fromList
    $ map (\(i, d) -> (s, i, d))
    $ Set.toList _elems

insertMonthlyPatternMain :: Statement (ScheduleId, MonthlyPatternState) ()
insertMonthlyPatternMain =
  lmap
    (\( ScheduleId i
      , PatternState (Pattern {_iterations}) (PatternPosition {_day, _index})
      ) -> (i, fromIntegral _iterations, fromIntegral _index, _day))
    [resultlessStatement|
    insert into monthly_pattern (id, iterations, elem_index, scheduled)
    values ($1 :: uuid, $2 :: int4, $3 :: int4, $4 :: date) |]

insertMonthlyPatternElems
  :: Statement (V.Vector (ScheduleId, Int, DayOfMonth)) ()
insertMonthlyPatternElems = Statement sql encoder Decoders.noResult True
  where
    sql =
      "insert into monthly_pattern_elem (monthly_pattern_id, iteration, point)\
      \select * from unnest ($1, $2, $3)"
    vector =
      Encoders.param
        . Encoders.nonNullable
        . Encoders.array
        . Encoders.dimension V.foldl'
        . Encoders.element
        . Encoders.nonNullable
    encoder = contramap V.unzip3 $ contrazip3
      (vector $ unScheduleId >$< Encoders.uuid)
      (vector $ fromIntegral >$< Encoders.int4)
      (vector $ fromIntegral . unDayOfMonth >$< Encoders.int4)

insertMonthlyPattern :: (ScheduleId, MonthlyPatternState) -> Session.Session ()
insertMonthlyPattern args@(s, PatternState Pattern {_elems} _) = do
  Session.statement args insertMonthlyPatternMain
  flip Session.statement insertMonthlyPatternElems
    $ V.fromList
    $ map (\(i, d) -> (s, i, d))
    $ Set.toList _elems

insertSchedule :: (ChoreId, ScheduleState) -> Session.Session ScheduleId
insertSchedule args@(_, s) = do
  scheduleId <- Session.statement args insertConstructor
  case s of
    FlexDaysSS x -> Session.statement (scheduleId, x) insertFlexDays
    StrictDaysSS x -> Session.statement (scheduleId, x) insertStrictDays
    WeeklyPatternSS x -> insertWeeklyPattern (scheduleId, x)
    MonthlyPatternSS x -> insertMonthlyPattern (scheduleId, x)
  return scheduleId

getSchedule :: Statement ScheduleId Schedule
getSchedule =
  dimap unScheduleId decoder [singletonStatement|
    select
      s.type :: text,
      fd.days :: int4?,
      sd.days :: int4?,
      wp_row.iterations :: int4?,
      wp_row.elem_iterations :: int4[]?,
      wp_row.elem_points :: int4[]?,
      mp_row.iterations :: int4?,
      mp_row.elem_iterations :: int4[]?,
      mp_row.elem_points :: int4[]?
    from schedule s
    left join flex_days fd on fd.id = s.id
    left join strict_days sd on sd.id = s.id
    left join (
      select
        wp.id,
        wp.iterations,
        array_agg(iteration) elem_iterations,
        array_agg(point) elem_points
      from weekly_pattern wp
      left join weekly_pattern_elem wpe on wp.id = wp.id
      group by wp.id, wpe.weekly_pattern_id
    ) wp_row on wp_row.id = s.id
    left join (
      select
        mp.id,
        mp.iterations,
        array_agg(iteration) elem_iterations,
        array_agg(point) elem_points
      from monthly_pattern mp
      left join monthly_pattern_elem mpe on mp.id = mp.id
      group by mp.id, mpe.monthly_pattern_id
    ) mp_row on mp_row.id = s.id
    where s.id = $1 :: uuid |]
  where
    decoder :: (
      Text,
      Maybe Int32,
      Maybe Int32,
      Maybe Int32,
      Maybe (V.Vector Int32),
      Maybe (V.Vector Int32),
      Maybe Int32,
      Maybe (V.Vector Int32),
      Maybe (V.Vector Int32)
      ) -> Schedule
    decoder ("flex_days", Just days, _, _, _, _, _, _, _) =
      FlexDaysS $ FlexDays $ fromIntegral days
    decoder ("strict_days", _, Just days, _, _, _, _, _, _) =
      StrictDaysS $ StrictDays $ fromIntegral days
    decoder ("weekly_pattern", _, _, Just i, Just ei, Just ep, _, _, _) =
      let
        elemIterations = map fromIntegral $ V.toList ei
        elemDays = map (toEnum . fromIntegral) $ V.toList ep
        elems = Set.fromList $ zip elemIterations elemDays
      in WeeklyPatternS $ Pattern elems $ fromIntegral i
    decoder ("monthly_pattern", _, _, _, _, _, Just i, Just ei, Just ep) =
      let
        elemIterations = map fromIntegral $ V.toList ei
        elemDays = map (DayOfMonth . fromIntegral) $ V.toList ep
        elems = Set.fromList $ zip elemIterations elemDays
      in MonthlyPatternS $ Pattern elems $ fromIntegral i
    decoder _ = error "impossible due to DB constraints"

deleteSchedule :: Statement ScheduleId ()
deleteSchedule =
  lmap unScheduleId
  [resultlessStatement| delete from schedule where id = $1 :: uuid|]
