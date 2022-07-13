{-# LANGUAGE OverloadedStrings #-}

module Page.Common where

import Servant.Links
import Lucid
import qualified Data.Text as T

import Page.Attribute
import Routes.Root
import Routes.SessionAuth

htmxScript :: Html ()
htmxScript =
  script_
    [ src_ "https://unpkg.com/htmx.org@1.7.0",
      integrity_ "sha384-EzBXYPt0/T6gxNp0nuPtLkmRpmDBbjg6WmCUZRLXBBwYYmwAUxzlSGej0ARHX0Bo",
      crossOrigin_ "anonymous"
    ]
    ("" :: T.Text)

container :: T.Text -> Html () -> Html ()
container title body =
  html_ $ do
    head_ $ do
      title_ $ toHtml title
      --link_ [rel_ "stylesheet", type_ "text/css", href_ "screen.css"]
      script_ [src_ "https://cdn.tailwindcss.com"] ("" :: T.Text)
      script_ [src_ "https://unpkg.com/alpinejs@3.x.x/dist/cdn.min.js"] ("" :: T.Text)
      htmxScript
      style_ "body{background:white}"
    body_ [class_ "flex"]$ do
      nav_ [class_ "flex-initial h-screen p-4 bg-slate-300"] $ ul_ $ do
        li_ $ a_ [href_ $ T.pack $ show $ linkURI $ _home rootLinks] "home"
        li_ $ a_ [href_ $ T.pack $ show $ linkURI $ _households rootLinks] "households"
        li_ $ a_ [href_ $ T.pack $ show $ linkURI $ _sessionLogout $ _session rootLinks] "logout"

      div_ [class_ "flex-1 p-4"] body

myInput :: [Attributes] -> Html ()
myInput = input_ . (<>) [type_ "text", class_ style]
  where style = "shadow appearance-none border rounded py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"

myButton :: [Attributes] -> Html a -> Html a
myButton = button_ . (<>) [class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"]

mySelect :: [Attributes] -> [(T.Text, T.Text)] -> Html ()
mySelect a items = select_ a $ mconcat $ flip map items $ \(value, name) ->
  option_ [value_ value] $ toHtml name
