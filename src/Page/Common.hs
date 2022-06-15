{-# LANGUAGE OverloadedStrings #-}

module Page.Common where

import Lucid
import Data.Text

import Page.Attribute

htmxScript :: Html ()
htmxScript =
  script_
    [ src_ "https://unpkg.com/htmx.org@1.7.0",
      integrity_ "sha384-EzBXYPt0/T6gxNp0nuPtLkmRpmDBbjg6WmCUZRLXBBwYYmwAUxzlSGej0ARHX0Bo",
      crossOrigin_ "anonymous"
    ]
    ("" :: Text)

container :: Text -> Html () -> Html ()
container title body =
  html_ $ do
    head_ $ do
      title_ $ toHtml title
      --link_ [rel_ "stylesheet", type_ "text/css", href_ "screen.css"]
      htmxScript
      style_ "body{background:white}"
    body_ body
