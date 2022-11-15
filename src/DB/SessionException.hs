module DB.SessionException where

import Control.Monad.Catch
import Control.Monad.Error.Class
import Hasql.Connection hiding (acquire, release)
import Hasql.Session

-- from https://github.com/nikita-volkov/hasql/issues/144

-- pseudo-newtype coercion helpers

wrapSession :: ReaderT Connection (ExceptT QueryError IO) a -> Session a
wrapSession m = do
  conn <- ask
  res <- liftIO $ runExceptT $ runReaderT m conn
  either throwError pure res

unwrapSession :: Session a -> ReaderT Connection (ExceptT QueryError IO) a
unwrapSession m = ReaderT $ \conn -> ExceptT $ run m conn

-- exceptions

instance MonadThrow Session where
  throwM = wrapSession . throwM

instance MonadCatch Session where
  catch action handler =
    wrapSession $ catch (unwrapSession action) (unwrapSession . handler)

instance MonadMask Session where
  mask f = wrapSession $
    mask $
      \u -> unwrapSession $ f (wrapSession . u . unwrapSession)

  uninterruptibleMask f = wrapSession $
    uninterruptibleMask $
      \u -> unwrapSession $ f (wrapSession . u . unwrapSession)

  generalBracket acquire release use =
    wrapSession $
      generalBracket
        (unwrapSession acquire)
        ((unwrapSession .) . release)
        (unwrapSession . use)
