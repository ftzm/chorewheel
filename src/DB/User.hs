{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards          #-}

module DB.User where

import Data.Functor.Contravariant ((>$<))
import Data.Profunctor (dimap)
import Hasql.Statement (Statement(..))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.TH
import Models (User(..), UserId(..))

insertUser :: Statement User UserId
insertUser =
  dimap (\User{..} -> (name, email)) UserId
  [singletonStatement|
    insert into "user" (name, email)
    values ($1 :: text, $2 :: text)
    returning id :: int4|]

selectUser :: Statement UserId User
selectUser =
  dimap unUserId (uncurry User)
  [singletonStatement|
    select name :: text, email :: text from "user" where id = $1 :: int4|]

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
  decoder = fmap UserId $ D.singleRow $ (D.column . D.nonNullable) D.int4

selectUser' :: Statement UserId User
selectUser' = Statement sql encoder decoder True where
  sql = "select name, email from \"user\" u where u.id = $1"
  encoder = unUserId >$< E.param (E.nonNullable E.int4)
  decoder = D.singleRow row where
    row = User
      <$> (D.column $ D.nonNullable D.text)
      <*> (D.column $ D.nonNullable D.text)
