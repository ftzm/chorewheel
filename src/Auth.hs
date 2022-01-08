{-# LANGUAGE ScopedTypeVariables #-}

module Auth where

-------------------------------------------------------------------------------
import Crypto.Random.Types (getRandomBytes)
import Data.ByteString.Base64 (encode)
--import Data.ByteString.Char8 (unpack)
import Data.ByteString (ByteString)
-------------------------------------------------------------------------------

genToken :: IO ByteString
genToken = encode <$> getRandomBytes 64
