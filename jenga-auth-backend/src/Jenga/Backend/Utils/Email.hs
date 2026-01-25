{-# OPTIONS_GHC -fno-warn-orphans #-}
module Jenga.Backend.Utils.Email where

import Jenga.Backend.Utils.Query
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Common.Schema

import Lamarckian.Types
import Lamarckian.Render
import qualified Reflex.Dom.Core as Rfx
import Rhyolite.Email
import Rhyolite.DB.NotifyListen.Beam
import Database.Beam.AutoMigrate.Compat
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Schema
import Database.Beam.Query
import Database.Beam.Backend.SQL
import Network.Mail.Mime

import Control.Exception as CE
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Aeson
import Data.Proxy
import Data.Pool
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LBS

newtype AdminEmail = AdminEmail { getAdminEmail :: Address }


data MkEmail x = MkEmail
  { _mkEmail_subject :: T.Text
  , _mkEmail_body :: Rfx.StaticWidget x ()
  }

showPart :: Part -> String
showPart = \case { PartContent bs -> T.unpack . T.decodeUtf8 . LBS.toStrict $ bs ; _ -> ""} . partContent

-- | TODO(galen): dont use this everywhere but instead add to task list for Email runner
-- | also see sendEmailIfNotLocalOrUnsubscribed
sendEmailIfNotLocal :: MonadIO m => EmailConfig -> Mail -> m (Either T.Text ())
sendEmailIfNotLocal cfg mail = do
  route <- liftIO $ readFile "config/common/route"
  case isPrefixOf "http://localhost:" route || isInfixOf "ngrok-free.app" route of
    True -> do
      liftIO $ putStrLn "LOCALHOST EMAIL"
      liftIO . print . mailTo $ mail
      liftIO . putStrLn . take 1000  . mconcat . fmap showPart . mconcat . mailParts $ mail
      pure $ Right ()
    False -> do
      x :: (Either IOException (Either EmailError ())) <- liftIO $ CE.try $ sendEmail cfg mail
      liftIO $ print x
      case x of
        Left ioException -> pure $ Left (T.pack $ show ioException)
        Right (Left emailError) -> pure $ Left (T.pack $ show emailError)
        Right (Right ()) -> pure $ Right ()

sendEmailIfNotLocalOrUnsubscribed
  :: forall db m cfg .
     ( MonadIO m
     , Database Postgres db
     , HasJengaTable Postgres db Unsubscribe
     , HasConfig cfg BaseURL
     , HasConfig cfg EmailConfig
     , HasConfig cfg (Pool Connection)
     )
  => Mail
  -> ReaderT cfg m (Either T.Text ())
sendEmailIfNotLocalOrUnsubscribed mail = do
  cfg <- asksM
  (unsubbed :: PgTable Postgres db Unsubscribe) <- asksTableM
  isLocalHostEnv >>= \case
    True -> do
      liftIO $ putStrLn "LOCALHOST EMAIL"
      liftIO $ print . mailTo $ mail
      liftIO $ print . take 1000 . mconcat . fmap showPart . mconcat . mailParts $ mail
      liftIO $ putStrLn "--------------------------"
      pure $ Right ()
    False -> do
      emails <- withDbEnv $ allUnsubscribedEmails unsubbed
      let mail' = mail { mailTo = filter (\mTo -> not $ addressEmail mTo `elem` emails)  $ mailTo mail }
      case null (mailTo mail') of
        True -> pure $ Right ()
        False -> do
          x :: (Either IOException (Either EmailError ())) <- liftIO $ CE.try $ sendEmail cfg mail'
          liftIO $ print x
          case x of
            Left ioException -> pure $ Left (T.pack $ show ioException)
            Right (Left emailError) -> pure $ Left (T.pack $ show emailError)
            Right (Right ()) -> pure $ Right ()

allUnsubscribedEmails
  :: (Database Postgres db)
  => PgTable Postgres db Unsubscribe
  -> Pg [T.Text]
allUnsubscribedEmails tbl = fmap _unsubscribe_email <$> allUnsubscribed tbl

allUnsubscribed
  :: (Database Postgres db)
  => PgTable Postgres db Unsubscribe
  -> Pg [Unsubscribe Identity]
allUnsubscribed tbl = runSelectReturningList $ select $ all_ tbl

type HasJsonNotifyTbl be t n =
  ( BeamSqlBackend be
  , be ~ Postgres
  , Table t
  , FromBackendRow be (PrimaryKey t Identity)
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , HasNotification n t
  )

instance HasColumnType Mail where
  defaultColumnType _ = defaultColumnType $ Proxy @(PgJSON Mail)

instance HasSqlValueSyntax PgValueSyntax Mail where
  sqlValueSyntax mail = sqlValueSyntax $ PgJSON mail


type EmailM cfg db m n be =
  ( HasConfig cfg AdminEmail
  , HasConfig cfg (Pool Connection)
  , HasJengaTable Postgres db SendEmailTask
  , HasJsonNotifyTbl be SendEmailTask n
  )


newMkEmailHtml
  :: forall db be m cfg n x.
     ( MonadIO m
     , EmailM cfg db m n be
     )
  => [Address]
  -> MkEmail x
  -> ReaderT cfg m (Either T.Text ())
newMkEmailHtml toPlural mkEmail = do --subject widget = do
  newEmailHtml @db toPlural (_mkEmail_subject mkEmail) (_mkEmail_body mkEmail)

buildNewEmailHtml
  :: forall m cfg x.
     ( MonadIO m
     -- , EmailM cfg db m n be
     , HasConfig cfg AdminEmail
     )
  => [Address]
  -> T.Text
  -> Rfx.StaticWidget x ()
  -> ReaderT cfg m [Mail]
buildNewEmailHtml toPlural subject widget = do
  body <- liftIO $ fmap snd $ Rfx.renderStatic widget
  from <- getAdminEmail <$> asksM -- toAddress <$> asksCfg _emailSendConfig
  forM toPlural $ \to -> do
    liftIO $ simpleMail to from subject "" (LT.fromStrict . T.decodeUtf8 $ body) []
  --pure $ Right ()


-- | TODO(galen) can we run ReaderT cfg in the context of StaticWidget?
newEmailHtml
  :: forall db be m cfg n x.
     ( MonadIO m
     , EmailM cfg db m n be
     )
  => [Address]
  -> T.Text
  -> Rfx.StaticWidget x ()
  -> ReaderT cfg m (Either T.Text ())
newEmailHtml toPlural subject widget = do
  body <- liftIO $ fmap snd $ Rfx.renderStatic widget
  from <- getAdminEmail <$> asksM -- toAddress <$> asksCfg _emailSendConfig
  (emailTbl :: PgTable Postgres db SendEmailTask) <- asksTableM
  forM_ toPlural $ \to -> do
    mail <- liftIO $ simpleMail to from subject "" (LT.fromStrict . T.decodeUtf8 $ body) []
    fmap Right $ void $ withDbEnvQuiet $
      insertAndNotify emailTbl $ SendEmailTask
      { _sendEmailTask_id = default_
      , _sendEmailTask_finished = val_ False
      , _sendEmailTask_checkedOutBy = nothing_
      , _sendEmailTask_payload = val_ mail
      , _sendEmailTask_result = nothing_
      , _sendEmailTask_isUrgent = val_ (Just True)
      }
  pure $ Right ()

newHTemplateEmailHtml
  :: forall db m cfg k a r x be n.
     ( MonadIO m
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg AdminEmail
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => [Address]
  -> T.Text
  -> HTemplateVars k a
  -> (HTemplateRefs k a -> StaticWidget' r x ())
  -> ReaderT cfg m (Either T.Text ())
newHTemplateEmailHtml toPlural subject mappy mkDom = do
  --renderStaticTemplate' mappy $ widget
  body <- liftIO $ runStaticHTemplateWidget mappy mkDom
  --body <- liftIO $ fmap snd $ renderStatic widget
  from <- getAdminEmail <$> asksM
  (emailTbl :: PgTable Postgres db SendEmailTask) <- asksTableM
  forM_ toPlural $ \to -> do
    mail <- liftIO $ simpleMail to from subject "" (LT.fromStrict . T.decodeUtf8 $ body) []
    pool <- asksM
    fmap Right $ void $ runDb pool $
      insertAndNotify emailTbl $ SendEmailTask
      { _sendEmailTask_id = default_
      , _sendEmailTask_finished = val_ False
      , _sendEmailTask_checkedOutBy = nothing_
      , _sendEmailTask_payload = val_ mail
      , _sendEmailTask_result = nothing_
      , _sendEmailTask_isUrgent = val_ (Just True)
      }
  pure $ Right ()

newAdminEmail
  :: forall db m cfg be n.
     ( MonadIO m
     , HasConfig cfg AdminEmail
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl be SendEmailTask n
     --, Has (ComposeC ToJSON Identity) n
     )
  => T.Text
  -> T.Text
  -> ReaderT cfg m ()
newAdminEmail subject body = do
  from <- getAdminEmail <$> asksM
  newEmail @db @n from subject body

newEmail
  :: forall db n cfg be m.
     ( MonadIO m
     , HasConfig cfg AdminEmail
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     --, Has (ComposeC ToJSON Identity) n
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => Address
  -> T.Text
  -> T.Text
  -> ReaderT cfg m ()
newEmail to subject body = do
  from <- getAdminEmail <$> asksM -- toAddress <$> asksCfg _emailSendConfig
  let mail = simpleMail' to from subject . LT.fromStrict $ body
  -- Use produced mail to insert new SendEmailTask into the database
  -- Note that it does not reliably work to wakeup the worker within the
  -- transaction that emits the job, because it may not be visible to
  -- the worker until the transaction commits: This is a data race.
  --
  -- The solution is to rely on Postgres' NOTIFY semantics, which we handle
  -- in the notify handler.

  (emailTbl :: PgTable Postgres db SendEmailTask) <- asksTableM
  liftIO $ putStrLn "hey"
  void $ withDbEnv $
    insertAndNotify emailTbl $ SendEmailTask
    { _sendEmailTask_id = default_
    , _sendEmailTask_finished = val_ False
    , _sendEmailTask_checkedOutBy = nothing_
    , _sendEmailTask_payload = val_ mail
    , _sendEmailTask_result = nothing_
    , _sendEmailTask_isUrgent = val_ (Just True)
    }
