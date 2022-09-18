module Page.Chore where

import Lucid
import Lucid.Base

import Page.Attribute
import Page.Common
import Chore
import Models (HouseholdId(..))

choresPage :: HouseholdId -> [Chore] -> Html ()
choresPage householdId chores = container "chores " $ do
  case chores of
    [] -> span_ "This household has no chores :("
    _ -> ul_ [] $ mconcat $ map (li_ . toHtml . (.name)) chores
  hr_ []
  br_ []
  addChore householdId

addChore :: HouseholdId -> Html ()
addChore householdId = form_ [hxPost_ $ "/create_chore/" <> show (unHouseholdId householdId)] $ do
  p_ "Add a chore"
  br_ []
  myInput [placeholder_ "Chore Name", name_ "choreName", xData_ "{}"]
  br_ []
  br_ []
  mySelect [ name_ "scheduleType"
           , hxGet_ "/schedule_form"
           , hxTarget_ "#schedule"]
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

adjuster
  :: Text -- ^ target prefix
  -> Text -- ^ add url
  -> Text -- ^ remove url
  -> Int
  -> Html ()
adjuster targetPrefix addUrlPrefix removeUrlPrefix count= do
  case count of
    0 -> myButton [ type_ "submit"
                  , class_ "opacity-50 cursor-not-allowed"
                  , disabled_ ""] "remove"
    _ -> myButton [ type_ "submit"
                  , hxGet_ removeUrl
                  , hxTarget_ $ targetPrefix <> "-" <> show count
                  , hxSwap_ "outerHTML"
                  ] "remove"
  input_ [ name_ "interval"
         , style_ "display: none;"
         , value_ $ show $ count + 1
         ]
  <>
  myButton [ type_ "submit"
           , hxGet_ addUrl
           , hxTarget_ "#adjuster"
           , hxSwap_ "outerHTML"
           ] "add"
  where
    addUrl = addUrlPrefix <> show (count + 1)
    removeUrl = removeUrlPrefix <> show count

--------------------

createWeeklyForm :: Int -> Html ()
createWeeklyForm _ = do
  span_ [] "Create chore on a flexible repeating schedule."
  weekRow 0
  div_ [id_ "adjuster"] $ weekAdjuster 0

weekAdjuster :: Int -> Html ()
weekAdjuster = adjuster "#week" "/add_week_row/" "/remove_week_row/"

weekRow :: Int -> Html ()
weekRow i =
  ul_ [ id_ $ "week-" <> show i] $ li_ $ mconcat $ map weekDay [1..7]
  where weekDay day = input_ [ class_ "mr-1"
                             , type_ "checkbox"
                             , name_ "days"
                             , value_ $ show i <> "-" <> show day
                             ]

addWeekRow :: Int -> Html ()
addWeekRow newRow = do
  weekRow newRow
  div_ [id_ "adjuster"] $ weekAdjuster newRow

removeWeekRow :: Int -> Html ()
removeWeekRow targetRow = do
  div_ [ id_ "adjuster"
       , makeAttributes "hx-swap-oob" "true"
       ] $ weekAdjuster (targetRow - 1)

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
    labelText = toHtml @Text $ show dayIndex
    inputValue = show monthIndex <> "-" <> show dayIndex
    labelClasses = unwords
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
  span_ $ toHtml @Text $ "Month " <> show i
  mconcat $ map (ul_ . mconcat) $ chunks 7 $ map (monthDay i) [1..31]
  where
    rowId = "month-" <> show i

monthAdjuster :: Int -> Html ()
monthAdjuster = adjuster "#month" "/add_month_row/" "/remove_month_row/"

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
  monthRow 0
  div_ [id_ "adjuster"] $ monthAdjuster 0
