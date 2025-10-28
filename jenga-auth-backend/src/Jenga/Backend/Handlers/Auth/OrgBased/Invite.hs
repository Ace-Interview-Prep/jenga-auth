module Jenga.Backend.Handlers.Auth.OrgBased.Invite where

-- import Backend.Config
-- then the common fields will be a Class
--import Backend.DB
import Jenga.Backend.Handlers.Auth.UserSignup (userSignup)
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Backend.DB.Instances ()
import Jenga.Common.BeamExtras
import Jenga.Common.Errors
import Jenga.Common.Schema
import Jenga.Common.Auth

import Rhyolite.Account
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Class
import Reflex.Dom.Core
import Database.Beam.Postgres
import Database.Beam.Schema

import Web.ClientSession as CS
import Data.Pool
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Signed
import Data.Bifunctor
import qualified Data.Text as T

inviteHandler
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
  => T.Text
  -> Id Account
  -> frontendRoute (Signed PasswordResetToken)
  -> T.Text
  -> (T.Text -> T.Text -> StaticWidget x ())  -- ^ email body
  -> ReaderT cfg m (Either (BackendError InviteError) ())
inviteHandler email inviter resetRoute subject mkBody = do
  user <- withDbEnv $ [iquery|
    select a.account_email from accounts a
      where a.account_id = ${inviter}
      limit 1
  |]
  let (inviterEmail) = case user of
        [] -> "another user"
        (Only e:_) -> e
  signupRes <- userSignup @db @beR
    subject
    email
    resetRoute
    (\link_ -> mkBody inviterEmail link_)
  pure $ first (fmap InviteSignupFailed) signupRes


    -- (\link_ -> do
    --     text $
    --       "You have been invited by "
    --        <> inviterEmail
    --        <> " to join Ace Talent Community. Follow the link and choose your password."
    --     el "div" $ do
    --       text link_
    -- )
