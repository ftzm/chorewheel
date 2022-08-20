{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module ApiUtil where

import Web.Cookie
import Control.Monad.Error.Class
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Servant
import Data.List (lookup)

getCookie :: Maybe Text -> ByteString -> Maybe ByteString
getCookie cookies n = lookup n . parseCookies . encodeUtf8 =<< cookies

justOrErr :: MonadError e m => e -> Maybe a -> m a
justOrErr e = maybe (throwError e) pure

defCookie :: ByteString -> ByteString -> SetCookie
defCookie k v =
  def
  { setCookieName = k
  , setCookieValue = v
  , setCookieHttpOnly = True
  , setCookieSameSite = Just sameSiteStrict
  , setCookiePath = Just "/"
  }

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0

removeCookie :: ByteString -> SetCookie
removeCookie k = (defCookie k "") { setCookieExpires = Just epoch}

-- https://github.com/haskell-servant/servant-auth/issues/146
type Post303 (cts :: [Type]) (hs :: [Type]) a = Verb 'POST 303 cts (Headers (Header "Location" Text ': hs) a)

-- https://github.com/haskell-servant/servant-auth/issues/146
type Get303 (cts :: [Type]) (hs :: [Type]) a = Verb 'GET 303 cts (Headers (Header "Location" Text ': hs) a)
