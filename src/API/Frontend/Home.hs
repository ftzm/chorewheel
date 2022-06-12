{-# LANGUAGE OverloadedStrings #-}

module API.Frontend.Home where

import Lucid
import Lucid.Base
import Data.Text

hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"

crossOrigin_ :: Text -> Attributes
crossOrigin_ = makeAttributes "crossorigin"

home :: Html ()
home =
  html_ $ do
    head_ $ do
      title_ "Home"
      link_ [rel_ "stylesheet", type_ "text/css", href_ "screen.css"]
      script_
        [ src_ "https://unpkg.com/htmx.org@1.7.0"
        , integrity_ "sha384-EzBXYPt0/T6gxNp0nuPtLkmRpmDBbjg6WmCUZRLXBBwYYmwAUxzlSGej0ARHX0Bo"
        , crossOrigin_ "anonymous"
        ]
        ("" :: Text)
      style_ "body{background:white}"
    body_ $ do
      div_ [id_ "header", style_ "color:green"] "Home"
      p_ $ span_ (strong_ "This is an example of Lucid syntax.")
      p_ $ span_ (strong_ "This will become a home page.")
      hr_ []
      a_ [href_ "/auth/session_logout"] "log out"
      ul_ $ mapM_ (li_ . toHtml . show) [1, 2, 3]
      table_ $
        tr_ $ do
          td_ "Hello!"
          td_ [class_ "alt"] "World!"
          td_ "Sup?"
