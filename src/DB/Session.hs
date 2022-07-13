{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE QuasiQuotes          #-}

module DB.Session where

import Hasql.Statement (Statement(..))
import Hasql.TH
import Data.Profunctor
import Data.Time.Clock

import Models

upsertToken :: Statement (UserId, SessionToken, UTCTime) ()
upsertToken =
  lmap (\(UserId i, SessionToken t, e) -> (i, t, e))
  [resultlessStatement|
    insert into session_token (user_id, token_string, expiry)
    values ($1 :: int4, $2 :: text, $3 :: timestamptz)
    on conflict (user_id) do update
    set token_string = EXCLUDED.token_string, expiry = EXCLUDED.expiry|]

extendToken :: Statement (SessionToken, UTCTime) (Maybe UserId)
extendToken =
  dimap (\(SessionToken t, e) -> (t,e)) (fmap UserId)
  [maybeStatement|
    UPDATE session_token
    SET expiry = ($2 :: timestamptz)
    WHERE token_string = ($1 :: text)
    AND expiry > now()
    RETURNING user_id :: int4|]

deleteToken :: Statement UserId ()
deleteToken =
  lmap unUserId
  [resultlessStatement| delete from session_token where user_id = $1 :: int4|]
