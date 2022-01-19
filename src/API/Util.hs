module API.Util where

import Data.ByteString
import Data.Text
import Data.Text.Encoding
import Web.Cookie
import Control.Monad.Error.Class
import Data.Maybe

getCookie :: Maybe Text -> ByteString -> Maybe ByteString
getCookie cookies n = lookup n =<< parseCookies . encodeUtf8 <$> cookies

justOrErr :: MonadError e m => e -> Maybe a -> m a
justOrErr e m = fromMaybe (throwError e) $ pure <$> m

defCookie :: ByteString -> ByteString -> SetCookie
defCookie k v =
  def
  { setCookieName = k
  , setCookieValue = v
  }
