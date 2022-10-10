module Page.Households where

import Lucid
-- import Lucid.Base
--
-- import Servant.Links
-- import Routes.Root
-- import Routes.SessionAuth

import Data.Time.Calendar (Day)

import Models
import Page.Attribute
import Page.Common
import Chore
import Schedule

householdsFragment :: [Household] -> Html ()
householdsFragment households =
  case households of
    [] -> span_ "You are not a member of any households :("
    hs -> mconcat $ flip map hs $ \Household{..} -> do
      li_ [class_ "m-2"] $ do
        span_ [class_ "mr-2"] $ toHtml name
        myButton [ hxPost_ $ "/household-leave/" <> show id'
                , hxTarget_ "#households"
                ] "Leave"

householdsPage :: [Household] -> Html ()
householdsPage households =
    container "Households" $ do
      div_ [id_ "header", style_ "color:green"] "Households"
      hr_ [class_ "mt-2 mb-2"]
      div_ [id_ "households"] $ householdsFragment households
      hr_ [class_ "mt-2 mb-2"]
      span_ "Add a new household:"
      form_ [hxPost_ "/household-create", hxTarget_ "#households"] $ do
        myInput [class_ "mr-2", name_ "newHouseholdName"]
        myButton [type_ "submit"] "Submit"

--------------------------------------------------------------------------------

data CellType = ResolutionCell Resolution | ScheduledCell User

householdPage
  :: ([Day], Day, [Day])
  -> ([(Chore, [Maybe CellType], Maybe CellType, [Maybe CellType])])
  -> Html ()
householdPage (pastDays, today, futureDays) choreRows =
  container "Household" $
  --table_ [class_ "table-auto border-collapse border border-slate-400 "] $ thead_ $ do
  table_ [class_ "table-auto"] $ thead_ $ do
    tr_ $ do
      th_  ""
      mconcat $ map (th_ [class_ "p-2 bg-slate-200 border border-slate-300"] . toHtml . show @Text) pastDays
      th_ [class_ "p-2 bg-blue-300 border border-slate-300"] . toHtml $ show @Text today
      mconcat $ map (th_ [class_ "p-2 bg-slate-200 border border-slate-300"] . toHtml . show @Text) futureDays
      mconcat $ flip map choreRows $ \(c, pds, t, fds) -> do
        tr_ $ do
          (th_ [class_ "p-2 bg-slate-100 border border-slate-300"] . toHtml . (.name)) c
          mconcat $ map (renderCell "") pds
          renderCell "bg-blue-100" t
          mconcat $ map (renderCell "") fds
  where
    renderCell :: Text -> Maybe CellType -> Html ()
    renderCell bg c = case c of
      Nothing ->
        td_ [class_ $ "p-2 border border-slate-300 text-center " <> bg]
        . toHtml @Text $ ""
      Just c' -> case c' of
        ResolutionCell _ ->
          td_ [class_ $ "p-2 border border-slate-300 text-center " <> bg]
          . toHtml @Text $ "resolved"
        ScheduledCell u ->
          td_ [class_ $ "p-2 border border-slate-300 text-center " <> bg]
          . toHtml @Text $ ((.name) u <> " ( )")
