module Jenga.Backend.Handlers.Auth.OrgBased.AdminSignup where

-- import Backend.DB (db)
-- import Backend.DB.OrgBased
-- import Backend.DB.UserInfo

-- --import Backend.Listen ()
-- import Backend.Config
-- import Backend.Utils.Email
-- import Backend.Utils.Log
-- import Common.Route
-- import Common.Schema
-- import Common.Types
--   ( LogItem(NewAdminUser)
--   )
import Jenga.Backend.DB.Auth
import Jenga.Backend.DB.OrgBased
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Schema
import Jenga.Common.Auth

import Obelisk.Route
import Rhyolite.Backend.Account
import qualified Reflex.Dom.Core as Rfx
import Network.Mail.Mime
import Database.Beam
import Database.Beam.Postgres
import Rhyolite.Account

import Web.ClientSession as CS
import Data.Pool
import Data.Signed
import Control.Monad.Trans.Reader
import Text.Email.Validate
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

adminSignupHandler
  :: forall db beR frontendRoute cfg m x n.
     ( MonadIO m
     , Database Postgres db
     , HasConfig cfg CS.Key
     , HasConfig cfg CompanySignupCode
     , HasConfig cfg BaseURL
     , HasConfig cfg AdminEmail
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrganizationEmails
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl Postgres SendEmailTask n
     )
  => NewCompanyEmail
  -> frontendRoute (Signed PasswordResetToken)
  -> (T.Text -> Rfx.StaticWidget x ())
  -> ReaderT cfg m (Either (BackendError AdminSignupError) ())
adminSignupHandler (NewCompanyEmail email orgName code) resetRoute mkEmail = do
  (acctTbl :: PgTable Postgres db Account) <- asksTableM
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  (orgTbl :: PgTable Postgres db OrganizationEmails) <- asksTableM

  matchesCompanyCodeEnv code >>= \case
    False -> pure $ Left . BUserError $ InvalidAdminCode
    True -> do
      -- signupAdmin emailConfig' emailSend csk dbConnection route email isUserType = do
      (isNew, aid) <- withDbEnv $ do
        ensureAccountExists' acctTbl $ T.decodeUtf8 . toByteString $ email
      case isNew of
        False -> pure $ Left . BUserError $ AlreadySignedUp
        True -> do
          mNonce <- withDbEnv $ do
            putAccountRelations uTypeTbl orgTbl aid (IsCompany orgName)
            newNonce acctTbl aid
          case mNonce of
            Nothing -> pure $ Left . BCritical $ FailedMkNonce_AdminSignup
            Just noncense -> do
              csk <- asksM
              resetToken <- withDbEnv $ passwordResetToken csk aid noncense
              link <- renderFullRouteFE @beR $ resetRoute :/ resetToken
              let
                to = Address
                     { addressName = Nothing
                     , addressEmail = T.decodeUtf8 . toByteString $ email
                     } -- recipients Address
              eRes <- newEmailHtml @db [to] "Admin Email Confirmation" $ mkEmail link
              case eRes of
                Left _ -> pure $ Left . BCritical $ FailedMkEmail_AdminSignup
                Right () -> do
                  pure $ Right ()

--  do
--                 Rfx.el "div" $ do
--                   Rfx.el "div" $ Rfx.text "New account creation requested"
--                   Rfx.el "div" $ Rfx.text "go the following address to set your password:"
--                   Rfx.el "div" $ Rfx.text link
--                   Rfx.el "div" $ Rfx.text ""
--                   Rfx.el "div" $ Rfx.text "if you didn't request this action, please ignore this email."
