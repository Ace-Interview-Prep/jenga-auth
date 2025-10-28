module Jenga.Backend.Handlers.Auth.UserSignup where

import Jenga.Backend.Utils.Account
import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.Schema

import Rhyolite.Account
import Network.Mail.Mime
import Database.Beam
import Database.Beam.Postgres
import Reflex.Dom.Core (StaticWidget)

import Data.Pool
import Web.ClientSession as CS
import Data.Signed
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Text.Email.Validate
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- userSignup
--   :: MonadIO m
--   => T.Text -- ^ email subject
--   -> T.Text -- ^ email address
--   -> frontendRoute (Signed PasswordResetToken)
--   -> (T.Text -> StaticWidget x ())
--   -> ReaderT cfg m (Either (BackendError UserSignupError) ())
-- userSignup subject address resetR mkEmail = userSignup'
--   subject
--   address
--   resetR
--   mkEmail


--let mkBody = body <> "\n" <> link -- plaintext emali
userSignup
  :: forall db beR be frontendRoute m cfg x n.
     ( MonadIO m
     , Database Postgres db
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasConfig cfg BaseURL
     , HasConfig cfg AdminEmail
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrganizationEmails
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => T.Text -- ^ email subject
  -> T.Text -- ^ email address
  -> frontendRoute (Signed PasswordResetToken)
  -> (T.Text -> StaticWidget x ())  -- ^ email body
  -> ReaderT cfg m (Either (BackendError UserSignupError) ())
userSignup subject email resetRoute mkBody = do
  case emailAddress (T.encodeUtf8 email) of
    Nothing -> pure . Left . BUserError $ BadSignupEmail
    Just emailParsed -> do
      createNewAccount @db @beR (emailParsed) (IsGroupUser emailParsed "Ace") resetRoute >>= \case
        Left e -> pure $ Left e
        Right link -> do
          let
            to = Address
              { addressName = Nothing
              , addressEmail = email
              }
          x <- newEmailHtml @db [to] subject $ mkBody link
          pure $ first (\_ -> BCritical NoEmailSent) x
