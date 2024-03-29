module Page.Landing where

import Lucid

import Page.Common

landingPage :: Html ()
landingPage =
  container "ChoreWheel" $ do
    div_ [id_ "header", style_ "color:green"] "Login or don't, it depends"
    hr_ []
    ul_ $ mapM_ (li_ . toHtml . show @Text) [1, 2, 3]
    table_ $
      tr_ $ do
        td_ "Hello!"
        td_ [class_ "alt"] "World!"
        td_ "Sup?"
