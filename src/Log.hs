{-# LANGUAGE UndecidableInstances #-}

module Log
  ( Katip()
  , KatipContext()
  , LogM
  , logError
  , logWarn
  , logInfo
  , logResource
  , logRequests
  ) where

import Data.Generics.Product.Typed
import Data.Generics.Internal.VL.Lens
import Control.Monad.Trans.Resource
import Katip

import Network.Wai
import Data.Aeson

--------------------------------------------------------------------------------
-- Generic Lens Katip Instances

instance
  ( MonadReader r m
  , HasType LogEnv r
  , MonadIO m
  ) => Katip m where
  getLogEnv = asks $ getTyped @LogEnv
  localLogEnv f = local (over (typed @LogEnv) f)

instance {-# OVERLAPPABLE #-}
  ( MonadReader r m
  , HasType LogEnv r
  , HasType LogContexts r
  , HasType Namespace r
  , MonadIO m
  ) => KatipContext m where
  getKatipContext = asks $ getTyped @LogContexts
  localKatipContext f = local (over (typed @LogContexts) f)
  getKatipNamespace = asks $ getTyped @Namespace
  localKatipNamespace f = local (over (typed @Namespace) f)

--------------------------------------------------------------------------------

-- Implementing katip as an effect and deriving in instance as for other
-- effects means that I can never get accurate code locations: it resolves
-- either to the module where the instance is defined or the module where
-- the instance is derived for the concrete monad. Instead we rely on the above
-- orphan instance and these helper functions to provide a nice interface that
-- abstracts a bit away from the Katip specifics.

type LogM = KatipContext

logError :: LogM m => HasCallStack => Text -> m ()
logError = withFrozenCallStack $ logLocM ErrorS . showLS

logWarn :: LogM m => HasCallStack => Text -> m ()
logWarn = withFrozenCallStack $ logLocM WarningS . showLS

logInfo :: LogM m => HasCallStack => Text -> m ()
logInfo = withFrozenCallStack $ logLocM InfoS . showLS

--------------------------------------------------------------------------------

logResource
  :: MonadResource m
  => Text
  -> Text
  -> m (LogEnv, LogContexts, Namespace)
logResource appName appEnv = snd <$> allocate create close
  where
    create = do
      handleScribe <- mkHandleScribeWithFormatter
        jsonFormat ColorIfTerminal stdout (permitItem InfoS) V2
      logEnv <- registerScribe "stdout" handleScribe defaultScribeSettings
        =<< initLogEnv (Namespace [appName]) (Environment appEnv)
      return (logEnv, mempty, "main")
    close (logEnv, _, _) = void $ closeScribes logEnv

--------------------------------------------------------------------------------
-- Request logging middleware

data RequestLog = RequestLog
  { method :: Text
  , path :: Text
  , query :: Text
  } deriving (Show, Generic)

instance ToJSON RequestLog

toRequestLog :: Request -> RequestLog
toRequestLog = RequestLog
  <$> decodeUtf8 . requestMethod
  <*> decodeUtf8 . rawPathInfo
  <*> decodeUtf8 . rawQueryString

logRequests :: LogEnv -> Namespace -> (Application -> Application)
logRequests logEnv namespace baseApp =
  \req responseFunc ->
    baseApp req $ \res -> logReq req >> responseFunc res
  where
    logReq req =
      runKatipContextT logEnv (sl "request" $ toRequestLog req) namespace
      $ logFM InfoS "request"
