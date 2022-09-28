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

householdPage :: [Day] -> [(Chore, [Maybe User])] -> Html ()
householdPage days chores =
  container "Household" $
  --table_ [class_ "table-auto border-collapse border border-slate-400 "] $ thead_ $ do
  table_ [class_ "table-auto"] $ thead_ $ do
    tr_ $ do
      th_  ""
      mconcat $ map (th_ [class_ "p-2 border border-slate-300"] . toHtml . show @Text) days
    mconcat $ flip map chores $ \(c, ss) -> do
      tr_ $ do
        (th_ [class_ "p-2 border border-slate-300"] . toHtml . (.name)) c
        mconcat $ map (th_ [class_ "p-2 border border-slate-300"] . toHtml . (\x -> maybe "" (.name)  x )) ss
