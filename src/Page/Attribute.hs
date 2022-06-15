{-# LANGUAGE OverloadedStrings #-}

module Page.Attribute where

import Lucid.Base
import Data.Text

hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"

crossOrigin_ :: Text -> Attributes
crossOrigin_ = makeAttributes "crossorigin"
