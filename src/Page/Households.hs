module Page.Households where

import Lucid

import Data.Time.Calendar (Day)

import Chore
import Models
import Page.Attribute
import Page.Common
import Routes.Root
import Schedule

householdsFragment :: [Household] -> Html ()
householdsFragment households =
  case households of
    [] -> span_ "You are not a member of any households :("
    hs -> mconcat $ flip map hs $ \Household{..} -> do
      li_ [class_ "m-2"] $ do
        span_ [class_ "mr-2"] $ toHtml name
        myButton
          [ hxPost_ $ show $ rootLinks.householdLeave id
          , hxTarget_ "#households"
          ]
          "Leave"

householdsPage :: [Household] -> Html ()
householdsPage households =
  container "Households" $ do
    div_ [id_ "header", style_ "color:green"] "Households"
    hr_ [class_ "mt-2 mb-2"]
    div_ [id_ "households"] $ householdsFragment households
    hr_ [class_ "mt-2 mb-2"]
    span_ "Add a new household:"
    form_
      [ hxPost_ $ show $ rootLinkFragments.householdCreate
      , hxTarget_ "#households"
      ]
      $ do
        myInput [class_ "mr-2", name_ "newHouseholdName"]
        myButton [type_ "submit"] "Submit"

--------------------------------------------------------------------------------

data CellType = ResolutionCell Resolution | ScheduledCell User
  deriving (Show)

gridButton :: HouseholdId -> ChoreId -> Day -> Bool -> Html ()
gridButton householdId choreId day completed =
  button_
    [ class_ $ mconcat ["ml-2"]
    , hxPost_ $ show $ _doChore rootLinks householdId choreId day
    , hxTarget_ $ "#chore-" <> show (unChoreId choreId)
    , hxSwap_ "outerHTML"
    ]
    symbol
 where
  symbol = if completed then "(x)" else "( )"

undoButton :: HouseholdId -> ChoreId -> Day -> Html ()
undoButton householdId choreId day =
  button_
    [ class_ $ mconcat ["ml-2"]
    , hxPost_ $ show $ _undoChore rootLinks householdId choreId day
    , hxTarget_ $ "#chore-" <> show (unChoreId choreId)
    , hxSwap_ "outerHTML"
    ]
    "undo"

gridCell :: Text -> HouseholdId -> ChoreId -> Day -> Maybe CellType -> Html ()
gridCell bg householdId choreId day c = case c of
  Nothing ->
    td_ [class_ $ "p-2 border border-slate-300 text-center " <> bg]
      . toHtml @Text
      $ ""
  Just c' -> case c' of
    ResolutionCell _ ->
      td_ [class_ $ "p-2 border border-slate-300 text-center " <> bg] $ do
        toHtml @Text $ "resolved"
        undoButton householdId choreId day
    ScheduledCell u ->
      td_ [class_ $ "p-2 border border-slate-300 text-center " <> bg] $ do
        toHtml @Text $ (.name) u
        gridButton householdId choreId day False

gridRow ::
  HouseholdId ->
  ([Day], Day, [Day]) ->
  (Chore, [Maybe CellType], Maybe CellType, [Maybe CellType]) ->
  Html ()
gridRow householdId (pastDays, today, futureDays) (c, pds, t, fds) =
  tr_ [id_ $ "chore-" <> show (unChoreId c.id')] $ do
    th_ [class_ "p-2 bg-slate-100 border border-slate-300"] . toHtml $ c.name
    mconcat $ zipWith (gridCell "" householdId c.id') pastDays pds
    gridCell "bg-blue-100" householdId c.id' today t
    mconcat $ zipWith (gridCell "" householdId c.id') futureDays fds

householdPage ::
  HouseholdId ->
  ([Day], Day, [Day]) ->
  [(Chore, [Maybe CellType], Maybe CellType, [Maybe CellType])] ->
  Html ()
householdPage householdId days@(pastDays, today, futureDays) choreRows =
  container "Household" $
    table_ [class_ "table-auto"] $ do
      thead_ $ do
        tr_ $ do
          th_ ""
          mconcat $ map thComp pastDays
          th_ [class_ "p-2 bg-blue-300 border border-slate-300"] . toHtml $
            show @Text today
          mconcat $ map thComp futureDays
      tbody_ $ do
        mconcat $ flip map choreRows $ \choreRow -> do
          gridRow householdId days choreRow
 where
  thComp :: Day -> Html ()
  thComp c =
    th_
      [class_ "p-2 bg-slate-200 border border-slate-300"]
      (toHtml $ show @Text c)
