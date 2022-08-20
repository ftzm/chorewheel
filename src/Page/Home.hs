{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Page.Home where

import Lucid
import Data.Text

import Servant.Links
import Routes.Root
import Routes.SessionAuth

import Page.Common

sessionLogoutLink :: Text
sessionLogoutLink = pack $ show $ linkURI $ _sessionLogout $ _session rootLinks

home :: Html ()
home =
  container "Home" $ do
    div_ [id_ "header", style_ "color:green"] "Home"
    p_ $ span_ (strong_ "This is an example of Lucid syntax.")
    p_ $ span_ (strong_ "This will become a home page.")
    hr_ []
    a_ [href_ sessionLogoutLink] "log out"
    ul_ $ mapM_ (li_ . toHtml . show @Text) [1, 2, 3]
    table_ $
      tr_ $ do
        td_ "Hello!"
        td_ [class_ "alt"] "World!"
        td_ "Sup?"
