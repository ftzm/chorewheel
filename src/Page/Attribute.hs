{-# LANGUAGE OverloadedStrings #-}

module Page.Attribute where

import Lucid.Base
import Data.Text

hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"

hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "hx-get"

hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "hx-target"

crossOrigin_ :: Text -> Attributes
crossOrigin_ = makeAttributes "crossorigin"

xData_ :: Text -> Attributes
xData_ = makeAttributes "x-data"
