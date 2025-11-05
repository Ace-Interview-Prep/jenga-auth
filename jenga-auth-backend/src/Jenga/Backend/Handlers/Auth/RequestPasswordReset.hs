module Jenga.Backend.Handlers.Auth.RequestPasswordReset where

-- import Backend.DB (db)
import Jenga.Backend.DB.Auth

-- import Common.Schema
-- import Common.Route
-- import Backend.Config
-- import Backend.Utils.Email
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.Schema

import Rhyolite.Backend.Account
import Rhyolite.Account
import Obelisk.Route
import Reflex.Dom.Core
import Database.Beam.Schema
import Database.Beam.Postgres (Postgres, Connection)

import Web.ClientSession as CS
import Network.Mail.Mime
import Data.Pool
import Data.Signed
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import qualified Text.Email.Validate as EmailValidate
import qualified Data.Text.Encoding as T

requestPasswordResetHandler
  :: forall db beR n cfg m frontendRoute.
     ( MonadIO m
     , Database Postgres db
     , HasConfig cfg CS.Key
     , HasConfig cfg BaseURL
     , HasConfig cfg AdminEmail
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl Postgres SendEmailTask n
     )
  => frontendRoute (Signed PasswordResetToken)
  -> Email
  -> ReaderT cfg m (Either (BackendError RequestPasswordResetError) ())
requestPasswordResetHandler resetRoute (Email rawEmail) = do
  csk <- asksM -- Cfg _clientSessionKey

  (acctTbl :: PgTable Postgres db Account) <- asksTableM
  let email' = (\x -> T.decodeUtf8 $ EmailValidate.toByteString x) <$> (EmailValidate.validate $ T.encodeUtf8 rawEmail)
  case email' of
    Left _ -> pure . Left . BUserError $ InvalidEmail
    Right email_ -> do
      isNew <- withDbEnv $ doesAccountExist acctTbl email_
      case isNew of
        Nothing -> pure . Left . BUserError $ NotSignedUp -- "An account with that email does not exist, please signup first"
        Just aid -> do
          mNonce <- withDbEnv $ newNonce acctTbl aid
          case mNonce of
            Nothing -> pure . Left . BCritical $ FailedMakeNonce
            Just noncense -> do
              token <- withDbEnv $ passwordResetToken csk aid noncense
              resetLink <- renderFullRouteFE @beR $ resetRoute :/ token
              let
                to = Address
                     { addressName = Nothing
                     , addressEmail = email_
                     } -- recipients Address
              _ <- newEmailHtml @db [to] "Password Reset Request" $ do
                el "div" $ do
                  el "div" $ text "Password reset requested"
                  el "div" $ text "go the following address to reset your password:"
                  el "div" $ text $ getLink resetLink
                  el "div" $ text "if you didn't request this action, please ignore this email."
              pure $ Right ()
