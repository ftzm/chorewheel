module ServantLucid (HTML) where

-- Servant.Lucid failed with an inscrutable typeclass error when I tried to use
-- its HTML type. As such, I'm using the below, which was taken from the
-- servant cookbook. Given that it's only a handful of lines I'm actually
-- inclined to prefer it to a dependency.

import Lucid
import Network.HTTP.Media ((//), (/:))
import Servant.API

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTML a where
  mimeRender _ = renderBS . toHtml
