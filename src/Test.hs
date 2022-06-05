{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Test where

import Control.Monad.Reader
import Data.Generics.Product.Typed
import GHC.Generics

-- Define your effect in the abstract like normal MTL style
class Monad m => ActionM m where
  doAction :: String -> m ()

-- Have some resource your effect depends on
data ActionResource = ActionResource deriving Show

-- Create a newtype we can define an implementation Against
newtype ActionT m a = ActionT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader r)

-- Define said concrete implementation
instance (MonadReader r m, HasType ActionResource r, MonadIO m) => ActionM (ActionT m) where
  doAction input = do
    actionThing <- asks $ getTyped @ActionResource
    liftIO $ putStrLn $ "Do " ++ input ++ "via" ++ (show actionThing)

-- Define a concrete Monad and derive an instance for it using DerivingVia
data AppEnv = AppEnv { actionResource :: ActionResource } deriving Generic
newtype App a = App (ReaderT AppEnv IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)
  deriving ActionM via (ActionT App)

-- Define another concrete monad, and get another instance for free
type AdHocEnv = (ActionResource, String, Int)
newtype App' a = App' (ReaderT AdHocEnv IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AdHocEnv)
  deriving ActionM via (ActionT App')

-- Things I don't like about this approach:
--   1. More indirection
--   2. More boilerplate
--
-- Things I _do_ like about this approach:
--   1. You define a concrete instance against its exact requirements rather
--      than a larger concrete monad, so it's more explicit.
--   2. It's trivial to derive instances for arbitary monad stacks that support
--      them. So you can create small, ad-hoc monad stacks in tests or
--      in other contexts.
--   3. The module defining the App Monad depends on the modules defining
--      the instance definitions, rather than the other way around.
