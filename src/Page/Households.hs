{-# LANGUAGE OverloadedStrings #-}

module Page.Households where

import Lucid
-- import Lucid.Base
import qualified Data.Text as T
--
-- import Servant.Links
-- import Routes.Root
-- import Routes.SessionAuth

import Models (Household(..), HouseholdId(..))
import Page.Attribute
import Page.Common

householdsFragment :: [(HouseholdId, Household)] -> Html ()
householdsFragment households =
  case households of
    [] -> span_ "You are not a member of any households :("
    hs -> mconcat $ flip map hs $ \(HouseholdId i, Household h) -> do
      li_ [class_ "m-2"] $ do
        span_ [class_ "mr-2"] $ toHtml h
        myButton [ hxPost_ $ T.pack $ "/household-leave/" ++ show i
                , hxTarget_ "#households"
                ] "Leave"

householdsPage :: [(HouseholdId, Household)] -> Html ()
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
