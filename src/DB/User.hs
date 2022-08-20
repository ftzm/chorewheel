{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards          #-}

module DB.User where

import Data.Functor.Contravariant ((>$<))
import Data.Profunctor (dimap, lmap)
import Hasql.Statement (Statement(..))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.TH
import Models (User(..), UserId(..))

insertUser :: Statement User ()
insertUser =
  lmap (\User{..} -> (unUserId id', name, email))
  [resultlessStatement|
    insert into "user" (id, name, email)
    values ($1 :: uuid, $2 :: text, $3 :: text)|]

selectUser :: Statement UserId User
selectUser =
  dimap unUserId (\(i, n, e) -> User (UserId i) n e)
  [singletonStatement|
    select id :: uuid, name :: text, email :: text
    from "user" where id = $1 :: uuid|]

-------------------------------------------------------------------------------
-- Examples of how to use the manual style

insertUser' :: Statement User UserId
insertUser' = Statement sql encoder decoder True where
  sql = "insert into \"user\" (name, email) \
        \values ($1, $2) \
        \returning id"
  encoder =
    (name >$< E.param (E.nonNullable E.text)) <>
    (email >$< E.param (E.nonNullable E.text))
  decoder = fmap UserId $ D.singleRow $ (D.column . D.nonNullable) D.uuid

selectUser' :: Statement UserId User
selectUser' = Statement sql encoder decoder True where
  sql = "select id, name, email from \"user\" u where u.id = $1"
  encoder = unUserId >$< E.param (E.nonNullable E.uuid)
  decoder = D.singleRow row where
    row = User
      <$> (fmap UserId $ D.column $ D.nonNullable D.uuid)
      <*> (D.column $ D.nonNullable D.text)
      <*> (D.column $ D.nonNullable D.text)
