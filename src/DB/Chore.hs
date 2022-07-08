{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module DB.Chore where

import Hasql.Statement (Statement(..))
import Hasql.TH
import Data.Profunctor (dimap)
import Data.Bifunctor (bimap)
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Int (Int32)
import qualified Data.Set as Set
import Data.Time.Calendar (Day)

import Models
import Chore
import Schedule
import Schedule.Pattern
import Schedule.Primitives

insertChore :: Statement (HouseholdId, Chore) ChoreId
insertChore =
  dimap (bimap unHouseholdId _name) ChoreId
  [singletonStatement|
    insert into chore (household_id, name)
    values ($1 :: int4, $2 :: text)
    returning id :: int4 |]

householdChores :: Statement HouseholdId (V.Vector (ChoreId, Chore))
householdChores =
  dimap unHouseholdId (fmap $ bimap ChoreId Chore)
  [vectorStatement|
    select c.id :: int4, c.name :: text
    from chore c
    where c.household_id = $1 :: int4
    order by name|]



getFullChoresByHousehold :: Statement HouseholdId (V.Vector (ChoreId, Chore, Schedule))
getFullChoresByHousehold =
  dimap unHouseholdId (V.map decoder) [vectorStatement|
    select
      c.id :: int4,
      c.name :: text,
      s.type :: text,
      fd.days :: int4?,
      sd.days :: int4?,
      sd.scheduled :: date?,
      wp_row.iterations :: int4?,
      wp_row.elem_iterations :: int4[]?,
      wp_row.elem_points :: int4[]?,
      wp_row.elem_index :: int4?,
      wp_row.scheduled :: date?,
      mp_row.iterations :: int4?,
      mp_row.elem_iterations :: int4[]?,
      mp_row.elem_points :: int4[]?,
      mp_row.elem_index :: int4?,
      mp_row.scheduled :: date?
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
    ) wp_row on wp_row.id = s.id
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
    ) mp_row on mp_row.id = s.id
    where c.household_id = $1 :: int4 |]
  where
    decoder :: (
      Int32,
      Text,
      Text,
      -- FlexDays
      Maybe Int32,
      -- StrictDays
      Maybe Int32,
      Maybe Day,
      -- WeeklyPattern
      Maybe Int32,
      Maybe (V.Vector Int32),
      Maybe (V.Vector Int32),
      Maybe Int32,
      Maybe Day,
      -- MonthlyPattern
      Maybe Int32,
      Maybe (V.Vector Int32),
      Maybe (V.Vector Int32),
      Maybe Int32,
      Maybe Day
      ) -> (ChoreId, Chore, Schedule)
    decoder t@(choreId, choreName, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =
      (ChoreId choreId, Chore choreName,) $ case t of
      (_, _, "flex_days", Just days, _, _, _, _, _, _, _, _, _, _, _, _) ->
        FlexDaysS $ FlexDays $ fromIntegral days
      (_, _, "strict_days", _, Just days, _, _, _, _, _, _, _, _, _, _, _) ->
        StrictDaysS $ StrictDays $ fromIntegral days
      (_, _, "weekly_pattern", _, _, _, Just i, Just ei, Just ep, _,  _, _, _, _, _, _) ->
        let
          elemIterations = map fromIntegral $ V.toList ei
          elemDays = map (toEnum . fromIntegral) $ V.toList ep
          elems = Set.fromList $ zip elemIterations elemDays
        in WeeklyPatternS $ Pattern elems $ fromIntegral i
      (_, _, "monthly_pattern", _, _, _, _, _, _, _, _, Just i, Just ei, Just ep, _, _) ->
        let
          elemIterations = map fromIntegral $ V.toList ei
          elemDays = map (DayOfMonth . fromIntegral) $ V.toList ep
          elems = Set.fromList $ zip elemIterations elemDays
        in MonthlyPatternS $ Pattern elems $ fromIntegral i
      _ -> error "impossible due to DB constraints"
