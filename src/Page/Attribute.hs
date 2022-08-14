{-# LANGUAGE OverloadedStrings #-}

module Page.Attribute where

import Lucid.Base
import Data.Text

crossOrigin_ :: Text -> Attributes
crossOrigin_ = makeAttributes "crossorigin"

-------------------------------------------------------------------------------
-- HTMX

hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"

hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "hx-get"

hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "hx-target"

hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "hx-swap"

-------------------------------------------------------------------------------
-- Alpine.js

xData_ :: Text -> Attributes
xData_ = makeAttributes "x-data"

xFor_ :: Text -> Attributes
xFor_ = makeAttributes "x-for"

xText_ :: Text -> Attributes
xText_ = makeAttributes "x-text"
