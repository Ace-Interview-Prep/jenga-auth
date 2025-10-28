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
--import Database.PostgreSQL.Simple.Class
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

--import Data.Constraint.Compose


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
      liftIO . print . mconcat . fmap showPart . mconcat . mailParts $ mail
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
  (unsubbed :: PgTable Postgres db Unsubscribe) <- asksTableM -- tableRef
  isLocalHostEnv >>= \case
    True -> do
      liftIO $ putStrLn "LOCALHOST EMAIL"
      liftIO $ print . mailTo $ mail
      liftIO $ print . mconcat . fmap showPart . mconcat . mailParts $ mail
      pure $ Right ()
    False -> do
      emails <- withDbEnv $ allUnsubscribedEmails unsubbed
      let mail' = mail { mailTo = filter (\mTo -> not $ addressEmail mTo `elem` emails)  $ mailTo mail }
      case null (mailTo mail') of
        True -> pure $ Right ()
        False -> do
          --unsubscribed <- runSerializable dbConnection $ allUnsubscribed
          x :: (Either IOException (Either EmailError ())) <- liftIO $ CE.try $ sendEmail cfg mail'
          liftIO $ print x
          case x of
            Left ioException -> pure $ Left (T.pack $ show ioException)
            Right (Left emailError) -> pure $ Left (T.pack $ show emailError)
            Right (Right ()) -> pure $ Right ()

-- minimal
--   :: ( MonadIO m
--      , HasJengaTable dbHost db Unsubscribe
--      )
--   => m ()
-- minimal = do
--   tableRef


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
  -- , Psql m
  -- , MonadBeam be m
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
    fmap Right $ void $ withDbEnv $
      insertAndNotify emailTbl $ SendEmailTask
      { _sendEmailTask_id = default_
      , _sendEmailTask_finished = val_ False
      , _sendEmailTask_checkedOutBy = nothing_
      , _sendEmailTask_payload = val_ mail
      , _sendEmailTask_result = nothing_
      , _sendEmailTask_isUrgent = val_ (Just True)
      }
  pure $ Right ()


-- newTemplateHtml :: HTemplateVars Int ->

-- newTemplateEmailHtml :: MonadIO m => [Address] -> T.Text -> TemplateVars -> StaticWidget' x () -> ReaderT cfg m (Either T.Text ())
-- newTemplateEmailHtml toPlural subject mappy widget = do
--   --renderStaticTemplate' mappy $ widget
--   body <- liftIO $ runEmailTemplateWidget mappy widget
--   --body <- liftIO $ fmap snd $ renderStatic widget
--   from <- toAddress <$> asksCfg _emailSendConfig
--   forM_ toPlural $ \to -> do
--     mail <- liftIO $ simpleMail to from subject "" (LT.fromStrict . T.decodeUtf8 $ body) []
--     pool <- asksCfg _dbPool
--     fmap Right $ void $ runDb pool $
--       insertAndNotify (_db_sendEmailTask db) $ SendEmailTask
--       { _sendEmailTask_id = default_
--       , _sendEmailTask_finished = val_ False
--       , _sendEmailTask_checkedOutBy = nothing_
--       , _sendEmailTask_payload = val_ mail
--       , _sendEmailTask_result = nothing_
--       , _sendEmailTask_isUrgent = val_ (Just True)
--       }
--   pure $ Right ()

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

-- data ActivityCase = NewDM | NewMention

-- data NewActivity = NewActivity
--   { _newActivity_toUser :: Maybe Username -- technically possible to message someone before they have done onboarding...TODO
--   , _newActivity_fromUser :: Maybe Username -- TODO: no Maybe?
--   , _newActivity_case :: ActivityCase
--   , _newActivity_toEmail :: Email
--   , _newActivity_msgContent :: T.Text
--   , _newActivity_roomName :: T.Text
--   }

-- newActivityEmail :: MonadIO m => NewActivity -> ReaderT cfg m (Either T.Text ())
-- newActivityEmail activity = do
--   roomLink <- renderFullRouteFE . roomNameToRoute . _newActivity_roomName $ activity
--   let senderName = fromMaybe "User" $ getUsername <$> _newActivity_fromUser activity
--   let subject = chooseSubjectPrefix (_newActivity_case activity) <> senderName
--   let to = [Address (getUsername <$> _newActivity_toUser activity) (unEmail $ _newActivity_toEmail activity)]
--   let vars = Map.fromList
--         [ ((roomLink, _newActivity_roomName activity), [(mkTmplValueA (_newActivity_msgContent activity) senderName)])
--         ]

--   newHTemplateEmailHtml to subject vars newMessagesInRoomTMPL
--   where
--     chooseSubjectPrefix = \case
--       NewDM -> "New direct message from user "
--       NewMention -> "New mention in Ace community by user "

-- newMentionEmail
--   :: MonadIO m
--   => Maybe Username
--   -> T.Text -- Name of receiver
--   -> Email -- Email -- TODO: make this and name of receiver an Address
--   -> T.Text -- Content
--   -> R FrontendRoute -- RoomLink
--   -> T.Text
--   -> ReaderT cfg m (Either T.Text ())
-- newMentionEmail senderUsername name email content roomRoute roomName = do
--   roomLink <- renderFullRouteFE roomRoute -- FrontendRoute_Channel :/ roomLink -- ("@" <> fromMaybe "Haskell" roomName)---(Just room)
--   let senderName = fromMaybe "User" $ getUsername <$> senderUsername
--   let subject = "New mention in Ace community by user " <> senderName
--   let to = [Address (Just name) (unEmail email)]
--   let vars = Map.fromList
--         [ ((roomLink, roomName), [(mkTmplValueA content senderName)])
--         ]

--   newHTemplateEmailHtml to subject vars newMessagesInRoomTMPL

-- newDMEmail
--   :: MonadIO m
--   => Maybe Username
--   -> Maybe Username
--   -> T.Text
--   -> T.Text
--   -> R FrontendRoute
--   -> T.Text
--   -> ReaderT cfg m (Either T.Text ())
-- newDMEmail senderUsername receiverUsername email content roomRoute roomName = do
--   roomLink <- renderFullRouteFE roomRoute
--   let messageSenderName = fromMaybe "User" $ getUsername <$> senderUsername -- (maybe "" ((<>) " from user: " . getUsername) senderUsername)
--   let subject = "New direct message from user " <> messageSenderName
--   let to [Address (getUsername <$> receiverUsername) email]

--   let vars = Map.fromList
--         [ ((roomLink, roomName), [(mkTmplValueA content senderName)])
--         ]

--   newHTemplateEmailHtml to subject vars newMessagesInRoomTMPL

-- type RoomInfo = (T.Text, T.Text)
-- type MessageInfo = T.Text --username
-- newMessagesInRoomTMPL :: Rfx.DomBuilder t m => Map.Map RoomInfo [(SlotKey, MessageInfo)] -> m ()
-- newMessagesInRoomTMPL vars = do
--   void $ flip Map.traverseWithKey vars $ \(roomLink, roomName) messages -> do  -- roomLink roomName messages writeMessage = do
--     elStyle "div" "border: 3px solid #444; padding: 15px; border-radius: 8px; background-color: #f9f9f9; max-width: 500px; margin: auto;" $ do
--       elStyle "div" "margin-bottom: 10px; font-weight: bold; font-size: 1.2em; color: #333;" $ do
--         text "Chatroom: " *> text roomName
--       elStyle "div" "border-top: 2px solid #ddd; padding-top: 10px;" $ do
--         forM_ messages $ \(msgID, senderName) -> messageTMPL senderName (T.pack msgID) templateSlot -- (_emailMsg_name msg) (T.pack messageSlotKey)
--     elAttr "a" ("href" =: roomLink) $ do
--       text "Respond in channel"

--   el "div" $ do
--     elAttr "div" ("style" =: "font-weight: bold;") $ text "Do not reply to this email"



-- messageTMPL :: Rfx.DomBuilder t m => T.Text -> T.Text -> (T.Text -> m ()) -> m ()
-- messageTMPL name message writeMessageOrSlot = do
--   elStyle "div" "padding: 10px; background: #fff; border: 1px solid #ddd; border-radius: 6px; margin-bottom: 8px;" $ do
--     elStyle "div" "font-weight: bold; color: #007bff;" $ do
--       text "From: " *> text name
--     elAttr "div" ("id" =: "templateRegion") $ elStyle "div" "padding: 8px; background: #f1f1f1; border-radius: 4px; margin-top: 5px;" $ do
--       writeMessageOrSlot message

-- elStyle :: Rfx.DomBuilder t m => T.Text -> T.Text -> m a -> m a
-- elStyle hTag sty = elAttr hTag ("style" =: sty)
