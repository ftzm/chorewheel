{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Page.Households where

import Lucid
-- import Lucid.Base
import qualified Data.Text as T
--
-- import Servant.Links
-- import Routes.Root
-- import Routes.SessionAuth

import Models (Household(..))
import Page.Attribute
import Page.Common

householdsFragment :: [Household] -> Html ()
householdsFragment households =
  case households of
    [] -> span_ "You are not a member of any households :("
    hs -> mconcat $ flip map hs $ \Household{..} -> do
      li_ [class_ "m-2"] $ do
        span_ [class_ "mr-2"] $ toHtml name
        myButton [ hxPost_ $ T.pack $ "/household-leave/" ++ show id'
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
