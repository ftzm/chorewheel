{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}

module Models where

import GHC.Generics

import Data.Aeson
import Data.Text (Text)
import Control.Monad.Trans

data User = User
  { name :: Text
  , email :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON User -- generated via Generic
instance FromJSON User -- generated via Generic

class Monad m => GetUsers m where
  getUsers :: m [User]

class Monad m => SetUser m where
  setUser :: User -> m String

type UserStore m =
  ( GetUsers m
  , SetUser m
  )

-- Pass-through instance for transformers
instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , GetUsers m
  ) => GetUsers (t m) where
  getUsers = lift getUsers
