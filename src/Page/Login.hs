module Page.Login where

import Lucid
--import Lucid.Base
import Data.Text

import Routes.Root
import Routes.SessionAuth

import Models (User(..))
--import Page.Attribute
import Page.Common

sessionLoginLink :: Text
sessionLoginLink = pack $ show $ _sessionLogin $ _session rootLinks


loginPage :: Html ()
loginPage =
    container "Log In" $ do
      div_ [id_ "header", style_ "color:green"] "Log In"
      hr_ []
      br_ []
      form_ [method_ "post", action_ sessionLoginLink] $ do
        myInput [class_ "mr-2", placeholder_ "username", name_ "_username"]
        myInput [class_ "mr-2", placeholder_ "password", name_ "_password"]
        myButton [type_ "submit"] "Submit"

loggedInPage :: User -> Html ()
loggedInPage user =
    container "Log in" $ do
      div_ [id_ "header", style_ "color:green"] "Already logged in"
      p_ $ span_ (strong_ $ toHtml $ "Hello " ++ unpack (name user) ++ ", You are already logged in.")
