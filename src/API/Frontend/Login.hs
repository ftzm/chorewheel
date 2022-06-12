{-# LANGUAGE OverloadedStrings #-}

module API.Frontend.Login where

import Lucid
import Lucid.Base
import Data.Text
import Models

hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"

crossOrigin_ :: Text -> Attributes
crossOrigin_ = makeAttributes "crossorigin"

loginPage :: Html ()
loginPage =
  html_ $ do
    head_ $ do
      title_ "Log In"
      link_ [rel_ "stylesheet", type_ "text/css", href_ "screen.css"]
      script_
        [ src_ "https://unpkg.com/htmx.org@1.7.0"
        , integrity_ "sha384-EzBXYPt0/T6gxNp0nuPtLkmRpmDBbjg6WmCUZRLXBBwYYmwAUxzlSGej0ARHX0Bo"
        , crossOrigin_ "anonymous"
        ]
        ("" :: Text)
      style_ "body{background:white}"
    body_ $ do
      div_ [id_ "header", style_ "color:green"] "Log In"
      p_ $ span_ (strong_ "Log In")
      hr_ []
      form_ [method_ "post", action_ "/auth/session"] $ do
        input_ [name_ "_username"]
        input_ [name_ "_password"]
        button_ [type_ "submit"] "Submit"

loggedInPage :: User -> Html ()
loggedInPage user =
  html_ $ do
    head_ $ do
      title_ "Log In"
      link_ [rel_ "stylesheet", type_ "text/css", href_ "screen.css"]
      script_
        [ src_ "https://unpkg.com/htmx.org@1.7.0"
        , integrity_ "sha384-EzBXYPt0/T6gxNp0nuPtLkmRpmDBbjg6WmCUZRLXBBwYYmwAUxzlSGej0ARHX0Bo"
        , crossOrigin_ "anonymous"
        ]
        ("" :: Text)
      style_ "body{background:white}"
    body_ $ do
      div_ [id_ "header", style_ "color:green"] "Already logged in"
      p_ $ span_ (strong_ $ toHtml $ "Hello " ++ unpack (name user) ++ ", You are already logged in.")
