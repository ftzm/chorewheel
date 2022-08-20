{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE QuasiQuotes          #-}

module DB.Password where

import Hasql.Statement (Statement(..))
import Hasql.TH
import Data.Profunctor

import Models

insertPassword :: Statement (UserId, PasswordHash) ()
insertPassword =
  lmap (bimap unUserId unPasswordHash)
  [resultlessStatement|
    insert into password (user_id, password_hash)
    values ($1 :: uuid, $2 :: text)|]

selectPassword :: Statement UserId PasswordHash
selectPassword =
  dimap unUserId PasswordHash
  [singletonStatement|
    select password_hash :: text from password where user_id = $1 :: uuid|]

passwordInfoByUsername :: Statement Username (Maybe (UserId, PasswordHash))
passwordInfoByUsername =
  dimap unUsername (fmap (bimap UserId PasswordHash))
  [maybeStatement|
    select u.id :: uuid, password_hash :: text
    from password p
    inner join "user" u on u.id = p.user_id
    where u.name = $1 :: text|]
