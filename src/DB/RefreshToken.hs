{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE QuasiQuotes          #-}

module DB.RefreshToken where

import Hasql.Statement (Statement(..))
import Hasql.TH
import Data.Text
import Data.Profunctor
import Data.Time.Clock

import Models

upsertToken :: Statement (UserId, Text, UTCTime) ()
upsertToken =
  lmap (\(UserId i, t, e) -> (i, t, e))
  [resultlessStatement|
    insert into refresh_token (user_id, token_string, expiry)
    values ($1 :: int4, $2 :: text, $3 :: timestamptz)
    on conflict (user_id) do update
    set token_string = EXCLUDED.token_string, expiry = EXCLUDED.expiry|]

selectToken :: Statement (Text, UTCTime) (Maybe UserId)
selectToken =
  rmap (fmap UserId)
  [maybeStatement|
    select user_id :: int4 from refresh_token
    where token_string = $1 :: Text and expiry > $2 :: timestamptz|]

deleteToken :: Statement UserId ()
deleteToken =
  lmap unUserId
  [resultlessStatement| delete from refresh_token where user_id = $1 :: int4|]
