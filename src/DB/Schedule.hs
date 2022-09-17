{-# LANGUAGE QuasiQuotes #-}

module DB.Schedule where


--import Data.Time.Calendar (Day)

import Chore
import Contravariant.Extras.Contrazip
import Data.Profunctor (dimap, lmap)
import qualified Data.Vector as V
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Statement (Statement (..))
import Hasql.TH
import qualified Data.Set.NonEmpty as NESet

import Schedule
import Schedule.Pattern
import Schedule.Primitives
import DB.Util

insertConstructor :: Statement (ChoreId, ScheduleState) ()
insertConstructor =
  lmap
    (bimap unChoreId toName)
    [resultlessStatement|
    insert into schedule (id, "type")
    values ($1 :: uuid, $2 :: text)
    returning id :: uuid |]
  where
    toName = \case
      FlexDaysSS _ -> "flex_days"
      StrictDaysSS _ -> "strict_days"
      WeeklyPatternSS _ -> "weekly_pattern"
      MonthlyPatternSS _ -> "monthly_pattern"
      UnscheduledSS -> "unscheduled"

insertFlexDays :: Statement (ChoreId, FlexDaysState) ()
insertFlexDays =
  lmap
    (\(ChoreId i, FlexDaysState (FlexDays n) d) -> (i, fromIntegral n, d))
    [resultlessStatement|
    insert into flex_days (id, days, scheduled)
    values ($1 :: uuid, $2 :: int4, $3 :: date) |]

insertStrictDays :: Statement (ChoreId, StrictDaysState) ()
insertStrictDays =
  lmap
    (\(ChoreId i, StrictDaysState (StrictDays n) d) -> (i, fromIntegral n, d))
    [resultlessStatement|
    insert into strict_days (id, days, scheduled)
    values ($1 :: uuid, $2 :: int4, $3 :: date) |]

insertWeeklyPatternMain :: Statement (ChoreId, WeeklyPatternState) ()
insertWeeklyPatternMain =
  lmap
    (\( ChoreId i
      , PatternState (Pattern {iterations}) (PatternPosition {day, index})
      ) -> (i, fromIntegral iterations, fromIntegral index, day))
    [resultlessStatement|
    insert into weekly_pattern (id, iterations, elem_index, scheduled)
    values ($1 :: uuid, $2 :: int4, $3 :: int4, $4 :: date) |]

insertWeeklyPatternElems :: Statement (V.Vector (ChoreId, Int, Weekday)) ()
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
      (vector $ unChoreId >$< Encoders.uuid)
      (vector $ fromIntegral >$< Encoders.int4)
      (vector $ fromIntegral . fromEnum >$< Encoders.int4)

insertWeeklyPattern :: (ChoreId, WeeklyPatternState) -> Session.Session ()
insertWeeklyPattern args@(s, PatternState Pattern {elems} _) = do
  Session.statement args insertWeeklyPatternMain
  flip Session.statement insertWeeklyPatternElems
    $ V.fromList
    $ map (\(i, d) -> (s, i, d))
    $ toList elems

insertMonthlyPatternMain :: Statement (ChoreId, MonthlyPatternState) ()
insertMonthlyPatternMain =
  lmap
    (\( ChoreId i
      , PatternState (Pattern {iterations}) (PatternPosition {day, index})
      ) -> (i, fromIntegral iterations, fromIntegral index, day))
    [resultlessStatement|
    insert into monthly_pattern (id, iterations, elem_index, scheduled)
    values ($1 :: uuid, $2 :: int4, $3 :: int4, $4 :: date) |]

insertMonthlyPatternElems
  :: Statement (V.Vector (ChoreId, Int, DayOfMonth)) ()
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
      (vector $ unChoreId >$< Encoders.uuid)
      (vector $ fromIntegral >$< Encoders.int4)
      (vector $ fromIntegral . unDayOfMonth >$< Encoders.int4)

insertMonthlyPattern :: (ChoreId, MonthlyPatternState) -> Session.Session ()
insertMonthlyPattern args@(s, PatternState Pattern {elems} _) = do
  Session.statement args insertMonthlyPatternMain
  flip Session.statement insertMonthlyPatternElems
    $ V.fromList
    $ map (\(i, d) -> (s, i, d))
    $ toList $ NESet.toList elems

insertSchedule :: (ChoreId, ScheduleState) -> Session.Session ()
insertSchedule args@(choreId, s) = do
  Session.statement args insertConstructor
  case s of
    FlexDaysSS x -> Session.statement (choreId, x) insertFlexDays
    StrictDaysSS x -> Session.statement (choreId, x) insertStrictDays
    WeeklyPatternSS x -> insertWeeklyPattern (choreId, x)
    MonthlyPatternSS x -> insertMonthlyPattern (choreId, x)
    UnscheduledSS -> return ()

getSchedule :: Statement ChoreId Schedule
getSchedule =
  dimap unChoreId decoder [singletonStatement|
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
        elems = loadNESetUnsafe $ zip elemIterations elemDays
      in WeeklyPatternS $ Pattern elems $ fromIntegral i
    decoder ("monthly_pattern", _, _, _, _, _, Just i, Just ei, Just ep) =
      let
        elemIterations = map fromIntegral $ V.toList ei
        elemDays = map (DayOfMonth . fromIntegral) $ V.toList ep
        elems = loadNESetUnsafe $ zip elemIterations elemDays
      in MonthlyPatternS $ Pattern elems $ fromIntegral i
    decoder ("unscheduled", _, _, _, _, _, _, _, _) = UnscheduledS
    decoder _ = error "impossible due to DB constraints"

deleteSchedule :: Statement ChoreId ()
deleteSchedule =
  lmap unChoreId
  [resultlessStatement| delete from schedule where id = $1 :: uuid|]

-------------------------------------------------------------------------

updateSchedule :: (ChoreId, ScheduleState) -> Session.Session ()
updateSchedule args@(choreId, _) =
  Session.statement choreId deleteSchedule >> insertSchedule args
