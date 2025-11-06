module Jenga.Backend.Handlers.Auth.OrgBased.AddUsers where

-- import Backend.DB.OrgBased
-- import Backend.Config
-- import Common.Types
-- import Backend.Utils.Account
import Jenga.Backend.Utils.Account
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.Email
import Jenga.Backend.DB.OrgBased
import Jenga.Common.Errors
import Jenga.Common.BeamExtras
import Jenga.Common.Schema
import Jenga.Common.Auth

import Rhyolite.Account
import Database.Beam.Schema
import Database.Beam.Postgres
import Snap

import Data.Pool
import Web.ClientSession as CS
import Data.Signed
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Applicative (some)
import Text.Parsec
import Text.Email.Validate
import qualified Data.Text.Encoding as T
import qualified Data.Text as T


type AddUsersConstraint db beR cfg be m n frontendRoute =
  --forall db beR cfg be m n frontendRoute x.
     ( MonadIO m
     , MonadSnap m
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


addUsersHandler
  :: forall db beR cfg be m n frontendRoute x.
     ( MonadIO m
     , MonadSnap m
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
  => Id Account
  -> T.Text
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -- ^ Email to send user
  -> ReaderT cfg m (Either (BackendError AddUsersError) ())
addUsersHandler acctID emails resetRoute mkEmail = do
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  case sepByCommas emails of
    Left _ -> pure $ Left . BUserError $ NoCommas -- "Error reading list, please ensure all emails are separated by commas"
    Right rawEmails -> do
      setTimeout $ length rawEmails + 10
      liftIO $ print rawEmails
      case sequenceA $ fmap (validate . T.encodeUtf8 . T.pack) rawEmails of
        Left _ -> pure $ Left . BUserError $ InvalidEmail_AddUser -- "Invalid email in list"
        Right emails' -> do
          results <- forM emails' $ \email -> do
            mOrgName <- withDbEnv $ getOrgName uTypeTbl acctID
            case mOrgName of
              Nothing -> pure $ Left . BUserError $ NoOrgCode $ T.pack . show $ acctID --"No organization code found"
              Just orgName -> do
                createNewAccountWithSetupEmail @db @beR email (IsGroupUser email orgName) resetRoute mkEmail >>= \case
                  Left bError -> pure $ Left $ AddUser_Signup <$> bError
                  Right a -> pure $ Right a
          pure $ () <$ sequenceA results

sepByCommas :: T.Text -> Either ParseError [String]
sepByCommas = parse p ""
  where
    p = sepBy (some $ noneOf [',']) (char ',')
