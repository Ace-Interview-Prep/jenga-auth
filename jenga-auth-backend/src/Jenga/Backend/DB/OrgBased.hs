module Jenga.Backend.DB.OrgBased where

-- import Backend.DB
-- import Backend.DB.UserInfo
import Jenga.Common.Schema
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.DB.Auth
import Jenga.Common.BeamExtras
-- import Common.Types
-- import Common.Schema
import Jenga.Common.Auth

import Rhyolite.Account
import Database.Beam.Postgres
import Database.Beam.Schema
import Database.Beam.Query

import Text.Email.Validate as EmailValidate
import Control.Monad
import Data.Functor.Identity
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

isOrgEmail
  :: Database Postgres db
  => PgTable Postgres db OrganizationEmails
  -> T.Text
  -> Pg Bool
isOrgEmail orgTbl email = do
  maybeValidEmail <- runSelectReturningOne $ select $ do
    orgEmails <- all_ orgTbl
    guard_ $ _validEmails_email orgEmails ==. (val_ email)
    pure orgEmails
  case maybeValidEmail of
    Just _ -> pure True
    Nothing -> pure False

putNewOrgEmail :: PgTable Postgres db OrganizationEmails -> NewUserEmail -> Pg ()
putNewOrgEmail orgTbl (NewUserEmail email orgFrom) = runInsert $ insert orgTbl $ insertExpressions
  [ OrganizationEmails (val_ $ T.toLower . T.decodeUtf8 . EmailValidate.toByteString $ email) (val_ orgFrom)
  ]

getThisUsersOrg
  :: Database Postgres db
  => PgTable Postgres db OrganizationEmails
  -> T.Text
  -> Pg (Maybe (OrganizationEmails Identity))
getThisUsersOrg orgTbl email = runSelectReturningOne $ select $ do
  orgEmails <- all_ orgTbl
  guard_ $ _validEmails_email orgEmails ==. (val_ email)
  pure orgEmails

getAdminOrgName
  :: Database Postgres db
  => PgTable Postgres db UserTypeTable
  -> Id Account
  -> Pg (Maybe T.Text)
getAdminOrgName = getOrgName
-- uTypeTbl aid = fmap join $ runSelectReturningOne $ select $ do
--   uTypes <- all_ uTypeTbl
--   guard_ $ _userType_acctID uTypes ==. (val_ $ acctIDtoInt64 aid)
--   pure $ _userType_companyID uTypes

getOrgName
  :: Database Postgres db
  => PgTable Postgres db UserTypeTable
  -> Id Account
  -> Pg (Maybe T.Text)
getOrgName uTypeTbl aid = do
  mUType <- runSelectReturningOne $ select $ do
    uTypes <- all_ uTypeTbl
    guard_ $ _userType_acctID uTypes ==. (val_ $ acctIDtoInt64 aid)
    pure uTypes
  pure $ join $ _userType_companyID <$> mUType

getOrgUsers
  :: Database Postgres db
  => PgTable Postgres db OrganizationEmails
  -> T.Text
  -> Pg [OrganizationEmails Identity]
getOrgUsers orgTbl orgName = runSelectReturningList $ select $ do
  orgUsers <- all_ orgTbl
  guard_ $ _validEmails_organizationFrom orgUsers ==. (val_ orgName)
  pure orgUsers

putAccountRelations
  :: Database Postgres db
  => PgTable Postgres db UserTypeTable
  -> PgTable Postgres db OrganizationEmails
  -> PrimaryKey Account Identity
  -> IsUserType
  -> Pg (Either UserSignupError ())
putAccountRelations uTypeTbl orgTable aid = \case
  IsGroupUser email orgName -> do
    tryPutGroupUser uTypeTbl orgTable aid email orgName
  IsSelf -> do
    putNewUserType uTypeTbl aid Nothing
    pure $ Right ()
  IsCompany orgName -> do
    putNewUserType uTypeTbl aid (Just orgName)
    pure $ Right ()
--  acctID | companyID  | userType
-- --------+------------+----------
--     106 | Ace        | Admin


tryPutGroupUser
  :: Database Postgres db
  => PgTable Postgres db UserTypeTable
  -> PgTable Postgres db OrganizationEmails
  -> PrimaryKey Account Identity
  -> EmailAddress
  -> T.Text
  -> Pg (Either UserSignupError ())
tryPutGroupUser uTypeTbl orgTable aid email orgName = do
  lookupCompanyInUserTypeTable uTypeTbl orgName >>= \case
    Nothing -> pure $ Left NoLinkedOrganization
    Just _ -> do
      putNewUserType uTypeTbl aid Nothing
      putNewOrgEmail orgTable (NewUserEmail email orgName)
      pure $ Right ()

lookupCompanyInUserTypeTable
  :: Database Postgres db
  => PgTable Postgres db UserTypeTable
  -> T.Text
  -> Pg (Maybe (UserTypeTable Identity))
lookupCompanyInUserTypeTable uTypeTbl orgName = do
  runSelectReturningOne $ select $ do
    uTypes <- all_ uTypeTbl
    guard_ $ _userType_userType uTypes ==. (val_ Admin)
    guard_ $ _userType_companyID uTypes ==. (val_ $ Just orgName)
    pure uTypes

getInviteLinkByCode
  :: Database Postgres db
  => PgTable Postgres db InviteLink
  -> T.Text
  -> Pg (Maybe (InviteLink Identity))
getInviteLinkByCode inviteTbl codeLink = do
  runSelectReturningOne $ select $ do
    filter_ (\link -> _inviteLink_code link ==. (val_ codeLink)) $ all_ inviteTbl

setLinkNumLeft
  :: PgTable Postgres db InviteLink
  -> T.Text
  -> Int64
  -> Pg ()
setLinkNumLeft inviteTbl codeLink numLeft = do
  runUpdate $ update
    inviteTbl
    (\inviteLink ->
       _inviteLink_numLeft inviteLink <-. (val_ $ Just numLeft)
    )
    (\inviteLink -> _inviteLink_code inviteLink ==. (val_ codeLink) &&. (isJust_ $ _inviteLink_numLeft inviteLink))
