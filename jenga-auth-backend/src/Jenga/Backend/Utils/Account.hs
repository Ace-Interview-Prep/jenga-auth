module Jenga.Backend.Utils.Account (createNewAccount, createNewAccountWithSetupEmail) where

import Jenga.Backend.DB.Auth
import Jenga.Backend.DB.OrgBased
import Jenga.Backend.Utils.HasTable (PgTable, HasJengaTable, asksTableM)
import Jenga.Common.Schema
import Control.Monad.Trans.Reader

import Web.ClientSession as CS

import Database.Beam.Postgres
import Database.Beam
--import Backend.DB (db)
-- import Backend.DB.UserInfo
-- import Backend.DB.OrgBased


-- import Common.Schema
-- import Common.Route
import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.HasConfig
import Jenga.Common.Errors
import Jenga.Common.Auth


import Rhyolite.Account
import Rhyolite.Backend.Account
import Obelisk.Route
import Reflex.Dom.Core as R
import Network.Mail.Mime
import Data.Signed

import Data.Pool
import Data.Bifunctor
import Text.Email.Validate
import qualified Data.Text as T
import qualified Data.Text.Encoding as T



createNewAccount
  :: forall db beR frontendRoute m cfg .
     ( MonadIO m
     , Database Postgres db
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasConfig cfg BaseURL
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrganizationEmails
     )
  => EmailAddress
  -> IsUserType
  -> frontendRoute (Signed PasswordResetToken)
  -> ReaderT cfg m (Either (BackendError UserSignupError) T.Text)
  -- TODO: use Link type with no exported constructor
createNewAccount email isUserType resetRoute = do
  csk <- asksM -- _clientSessionKey
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  (orgTbl :: PgTable Postgres db OrganizationEmails) <- asksTableM
  (acctsTbl :: PgTable Postgres db Account) <- asksTableM
  --routeEncoder <- asksM
  (accountsTable :: PgTable Postgres db Account) <- asksTableM
  (isNew, aid) <- withDbEnv $
    ensureAccountExists' acctsTbl $ T.decodeUtf8 . toByteString $ email
  --let (AccountId (SqlSerial rawID)) = aid
  -- emailCofoundersWithEnv $ LT.fromStrict . T.pack $
  --   "AccountID: " <> show rawID  <> " for email: " <> show email
  case isNew of
    False -> pure $ Left . BUserError $ AccountExists
    True -> do
      mNonce <- withDbEnv $ do
        putAccountRelations uTypeTbl orgTbl aid isUserType
        newNonce accountsTable aid
      case mNonce of
        Nothing -> pure $ Left . BUserError $ FailedMakeResetToken
        Just noncense -> do
          token <- withDbEnv $ passwordResetToken csk aid noncense
          resetLink <- renderFullRouteFE @beR $ ((resetRoute :/ token) :: R frontendRoute)
          --resetLink = renderFrontendRoute routeEncoder $ resetRoute :/ token
          pure $ Right resetLink

createNewAccountWithSetupEmail
  :: forall db beR cfg be m n frontendRoute x.
     ( MonadIO m
     --, EmailM cfg db m n be
     , HasJsonNotifyTbl be SendEmailTask n
     , Database Postgres db
     , HasConfig cfg AdminEmail
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasConfig cfg BaseURL
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrganizationEmails
     , HasJengaTable Postgres db SendEmailTask
     )
  => EmailAddress
  -> IsUserType
  -> frontendRoute (Signed PasswordResetToken)
  -> (T.Text -> StaticWidget x ())
  -> ReaderT cfg m (Either (BackendError UserSignupError)  ())
createNewAccountWithSetupEmail email isUserType resetRoute mkEmail = do
  (createNewAccount @db @beR email isUserType resetRoute) >>= (\case
    Left e -> pure $ Left e
    Right link_ -> do
      x <- newEmailHtml @db [to] "Email Confirmation" $ mkEmail link_
      pure $ first (\_ -> BCritical NoEmailSent) x)
  where
    to = Address
      { addressName = Nothing
      , addressEmail = T.decodeUtf8 . toByteString $ email
      }
