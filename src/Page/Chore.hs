{-# LANGUAGE OverloadedStrings #-}

module Page.Chore where

import Lucid

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
addChore = form_ $ do
  myInput [placeholder_ "Chore Name", name_ "name", xData_ "{}"]
  br_ []
  br_ []
  mySelect [name_ "form_type", hxGet_ "/schedule_form", hxTarget_ "#schedule"]
    [ ("unscheduled", "Unscheduled")
    , ("flex", "Flexible intervals")
    , ("strict", "Regular intervals")
    , ("weekly", "Weekly pattern")
    , ("monthly", "Monthly pattern")
    ]
  div_ [id_ "schedule"] unscheduled

unscheduled :: Html ()
unscheduled = span_ [] "Unscheduled"

createStrictForm :: Html ()
createStrictForm = form_ $ do
  span_ [] "Create chore on strict repeating schedule."
  br_ []
  myInput [placeholder_ "name"]
  myInput [placeholder_ "days"]
  myButton [type_ "submit"] "Submit"

createFlexForm :: Html ()
createFlexForm = form_ $ do
  span_ [] "Create chore on flexible repeating schedule."
  br_ []
  myInput [placeholder_ "name"]
  myInput [placeholder_ "days"]
  myButton [type_ "submit"] "Submit"
