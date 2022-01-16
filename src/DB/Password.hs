{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module DB.Password where

import Data.Functor.Contravariant ((>$<))
import Hasql.Statement (Statement(..))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Data.Int
import Data.Text

-- TODO: this shouldn't return anything
insertPassword :: Statement (Int32, Text) Int32
insertPassword = Statement sql encoder decoder True where
  sql = "insert into password (user_id, password_hash) \
        \values ($1, $2) \
        \returning id"
  encoder =
    (fst >$< E.param (E.nonNullable E.int4)) <>
    (snd >$< E.param (E.nonNullable E.text))
  decoder = D.singleRow $ (D.column . D.nonNullable) D.int4

selectPassword :: Statement Int32 Text
selectPassword = Statement sql encoder decoder True where
  sql = "select password_hash from password where user_id = $1"
  encoder = E.param (E.nonNullable E.int4)
  decoder = D.singleRow (D.column $ D.nonNullable D.text)
