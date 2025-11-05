module Jenga.Backend.Utils.ErrorHandling where

import Jenga.Common.Schema
import Jenga.Common.Errors
import Jenga.Common.Log
import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.Log
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.HasConfig
import Control.Monad.Trans.Reader
-- import Backend.Config
-- import Backend.Utils.Email
-- import Backend.Utils.Log
-- import Common.Types (LogItem(..))
import Reflex.Dom.Core
import Database.Beam.Postgres (Connection, Postgres)
import Data.Pool
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Text as T


withErrorReporting
  :: forall db cfg be n e m a.
     ( MonadIO m
     , MonadCatch m
     , Show e
     , SpecificError (BackendError e)
     , HasConfig cfg AdminEmail
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJengaTable Postgres db LogItemRow
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => ReaderT cfg m (Either (BackendError e) a)
  -> ReaderT cfg m (Either (BackendError e) a)
withErrorReporting mE =  do
  --result :: (Either SomeException (Either (BackendError e) a)) <- try mE
  tryBE mE >>= reportWhenError @db
  -- result <- tryBE mE
  -- reportWhenError $ joinError result

-- | Catches any exceptions and join them into a BackendError
tryBE :: MonadCatch m => m (Either (BackendError e) a) -> m (Either (BackendError e) a)
tryBE ma = try ma >>= pure . joinError


joinError :: Either SomeException (Either (BackendError e) a) -> (Either (BackendError e) a)
joinError = \case
  Left someException -> Left . BException . T.pack . show $ someException
  Right eithBErrorResult -> eithBErrorResult

newtype SomeError = SomeError T.Text

instance Loggable SomeError where
  renderLog (SomeError x) = x

reportWhenError
  :: forall db cfg be n e m a.
     ( MonadIO m
     , Show e
     , SpecificError (BackendError e)
     , HasConfig cfg AdminEmail
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJengaTable Postgres db LogItemRow
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => (Either (BackendError e) a) -> ReaderT cfg m (Either (BackendError e) a)
reportWhenError = \case
  Right a -> pure $ Right a
  Left (BException s) -> do
    let err = BException s
    adminEmail <- getAdminEmail <$> asksM
    void $ newEmailHtml @db [adminEmail] "BException" $ do
      text . T.pack . show $ err
    pure $ Left err
  Left (BCritical e) -> do
    let err = BCritical e
    adminEmail <- getAdminEmail <$> asksM
    void $ newEmailHtml @db [adminEmail] "BCritical" $ do
      text . T.pack . show $ err
    pure $ Left err
  Left (BUserError e) -> do
    let err = BUserError e
    reportLog @db False $ SomeError $ T.pack $ show err
    pure $ Left err
  Left (BInvalidRequest e) -> do
    let err = BInvalidRequest e
    reportLog @db False $ SomeError $ T.pack . show $ err
    pure $ Left err
  Left NoAuth -> do
    let err = NoAuth
    reportLog @db False $ SomeError "NoAuth"
    pure $ Left err
  Left NoAuth_NoCookie -> do
    let err = NoAuth
    reportLog @db False $ SomeError "NoAuth_NoCookie"
    pure $ Left err
  Left NoAuth_CantReadKey -> do
    let err = NoAuth
    reportLog @db False $ SomeError "NoAuth_CantReadKey"
    pure $ Left err
  Left NoAuth_NoUserTypeCookie -> do
    let err = NoAuth
    reportLog @db False $ SomeError "NoAuth_CantReadKey"
    pure $ Left err
