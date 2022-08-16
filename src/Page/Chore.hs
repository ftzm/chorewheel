{-# LANGUAGE OverloadedStrings #-}

module Page.Chore where

import Lucid
import Lucid.Base
import qualified Data.Text as T

import Page.Attribute
import Page.Common
import Chore
import Schedule

choresPage :: [(ChoreId, Chore, ScheduleState)] -> Html ()
choresPage chores = container "chores " $ do
  case chores of
    [] -> span_ "This household has no chores :("
    _ -> span_ "Damn son look at all those chores"
  hr_ []
  br_ []
  addChore

addChore :: Html ()
addChore = form_ [hxPost_ "/create_chore"] $ do
  p_ "Add a chore"
  br_ []
  myInput [placeholder_ "Chore Name", name_ "choreName", xData_ "{}"]
  br_ []
  br_ []
  mySelect [name_ "scheduleType", hxGet_ "/schedule_form", hxTarget_ "#schedule"]
    [ ("unscheduled", "Unscheduled")
    , ("flex", "Flexible intervals")
    , ("strict", "Regular intervals")
    , ("weekly", "Weekly pattern")
    , ("monthly", "Monthly pattern")
    ]
  div_ [id_ "schedule"] unscheduled
  myButton [type_ "submit"] "Submit"

unscheduled :: Html ()
unscheduled = span_ [] "Unscheduled"

createStrictForm :: Html ()
createStrictForm = do
  span_ [] "Create chore on strict repeating schedule."
  br_ []
  myInput [name_ "interval", placeholder_ ""]

createFlexForm :: Html ()
createFlexForm = do
  span_ [] "Create chore on flexible repeating schedule."
  br_ []
  myInput [name_ "interval", placeholder_ ""]


--------------------

--id_ "oi", makeAttributes "hx-swap-oob" "true", method_ "post"
createWeeklyForm :: Int -> Html ()
createWeeklyForm _ = do
  span_ [] "Create chore on a flexible repeating schedule."
  weekRow 1
  div_ [id_ "adjuster"] $ weekAdjuster 1

weekAdjuster :: Int -> Html ()
weekAdjuster weekCount = do
  case weekCount of
    1 -> myButton [type_ "submit", class_ "opacity-50 cursor-not-allowed", disabled_ ""] "remove"
    _ -> myButton [type_ "submit", hxGet_ (T.pack $ "/remove_week_row/" ++ show weekCount), hxTarget_ (T.pack $ "#week-" ++ show weekCount), hxSwap_ "outerHTML"] "remove"
  myButton [type_ "submit", hxGet_ (T.pack $ "/add_week_row/" ++ show (weekCount + 1)), hxTarget_ "#adjuster", hxSwap_ "outerHTML"] "add"


weekRow :: Int -> Html ()
weekRow i =
  ul_ [id_ $ "week-" <> T.pack (show i)] $ li_ $ mconcat $ map (\j -> input_ [class_ "mr-1", type_ "checkbox", name_ "days", value_ (T.pack $ show i ++ "-" ++ show j)]) [1..7]

addWeekRow :: Int -> Html ()
addWeekRow newRow = do
  weekRow newRow
  div_ [id_ "adjuster"] $ weekAdjuster newRow

removeWeekRow :: Int -> Html ()
removeWeekRow targetRow = do
  div_ [id_ "adjuster", makeAttributes "hx-swap-oob" "true"] $ weekAdjuster (targetRow - 1)


-----------

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

monthDay :: Int -> Int -> Html ()
monthDay monthIndex dayIndex =
  label_ $ do
    input_ [ type_ "checkbox"
           , name_ "days"
           , class_ "peer hidden"
           , value_ inputValue
           ]
    span_ [class_ labelClasses] labelText
  where
    labelText = toHtml $ T.pack $ show dayIndex
    inputValue = T.pack $ show monthIndex ++ "-" ++ show dayIndex
    labelClasses = T.pack $ unwords
      [ "mr-1"
      , "mt-1"
      , "h-8"
      , "w-8"
      , "pt-1"
      , "pl-2"
      , "inline-block"
      , "bg-gray-200"
      , "rounded-md"
      , "peer-checked:bg-blue-500"
      , "peer-checked:text-white"
      ]

monthRow :: Int -> Html ()
monthRow i = div_ [ id_ rowId, class_ "mb-2"] $ do
  span_ $ toHtml $ T.pack $ "Month " ++ show i
  mconcat $ map (ul_ . mconcat) $ chunks 7 $ map (monthDay i) [1..31]
  where
    rowId = T.pack $ "month-" ++ show i

monthAdjuster :: Int -> Html ()
monthAdjuster count = do
  case count of
    1 -> myButton [ type_ "submit"
                  , class_ "opacity-50 cursor-not-allowed"
                  , disabled_ ""] "remove"
    _ -> myButton [ type_ "submit"
                  , hxGet_ removeUrl
                  , hxTarget_ (T.pack $ "#month-" ++ show count)
                  , hxSwap_ "outerHTML"
                  ] "remove"
  myButton [ type_ "submit"
           , hxGet_ addUrl
           , hxTarget_ "#adjuster"
           , hxSwap_ "outerHTML"
           ] "add"
  where
    addUrl = T.pack $ "/add_month_row/" ++ show (count + 1)
    removeUrl = T.pack $ "/remove_month_row/" ++ show count

addMonthRow :: Int -> Html ()
addMonthRow newRow = do
  monthRow newRow
  div_ [id_ "adjuster"] $ monthAdjuster newRow

removeMonthRow :: Int -> Html ()
removeMonthRow targetRow =
  div_ [ id_ "adjuster"
       , makeAttributes "hx-swap-oob" "true"
       ] $ monthAdjuster (targetRow - 1)

createMonthlyForm :: Int -> Html ()
createMonthlyForm _ = do
  p_ "Create chore on a monthly repeating schedule."
  monthRow 1
  div_ [id_ "adjuster"] $ monthAdjuster 1
