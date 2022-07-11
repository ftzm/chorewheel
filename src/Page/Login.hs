{-# LANGUAGE OverloadedStrings #-}

module Page.Login where

import Lucid
--import Lucid.Base
import Data.Text

import Servant.Links
import Routes.Root
import Routes.SessionAuth

import Models (User(..))
--import Page.Attribute
import Page.Common

sessionLoginLink :: Text
sessionLoginLink = pack $ show $ linkURI $ _sessionLogin $ _session rootLinks

loginPage :: Html ()
loginPage =
    container "Log In" $ do
      div_ [id_ "header", style_ "color:green"] "Log In"
      p_ $ span_ (strong_ "Log In")
      hr_ []
      form_ [method_ "post", action_ sessionLoginLink] $ do
        input_ [name_ "_username"]
        input_ [name_ "_password"]
        button_ [type_ "submit"] "Submit"

loggedInPage :: User -> Html ()
loggedInPage user =
    container "Log in" $ do
      div_ [id_ "header", style_ "color:green"] "Already logged in"
      p_ $ span_ (strong_ $ toHtml $ "Hello " ++ unpack (name user) ++ ", You are already logged in.")
