module Jenga.Backend.DB.Emailing where

import Jenga.Backend.Utils.HasTable
import Jenga.Backend.DB.Auth
import Jenga.Common.Schema
import Jenga.Common.BeamExtras
import Jenga.Common.Auth

import Rhyolite.Account
import Database.Beam.Schema
import Database.Beam.Postgres
import Database.Beam.Query
import Database.Beam.Backend.SQL.Types -- (SqlSerial(..))

import Data.Functor.Identity
import qualified Data.Text as T
import Data.Int (Int64)


allUnsubscribed
  :: Database Postgres db
  => PgTable Postgres db Unsubscribe
  -> Pg [Unsubscribe Identity]
allUnsubscribed = runSelectReturningList . select . all_-- (_db_unsubscribe db)

allUnsubscribedEmails :: Database Postgres db => PgTable Postgres db Unsubscribe -> Pg [T.Text]
allUnsubscribedEmails tbl = fmap _unsubscribe_email <$> allUnsubscribed tbl

addUnsubscribed
  :: Database Postgres db
  => PgTable Postgres db Account
  -> PgTable Postgres db Unsubscribe
  -> Id Account
  -> Pg (Either UnsubscribeError ())
addUnsubscribed acctTbl unSubTbl user = do
  getUsersEmail acctTbl user >>= \case
    Nothing -> pure $ Left EmailNotFound -- "email not found"
    Just email -> do
      runInsert $ insert unSubTbl $ insertExpressions
        [ Unsubscribe (val_ $ acctIDtoSerial user) (val_ email) ]
      pure $ Right ()

-- Delete it
markEmailTaskComplete
  :: Database Postgres db
  => PgTable Postgres db SendEmailTask
  -> SqlSerial Int64
  -> Pg ()
markEmailTaskComplete emailTbl emailTaskId = runDelete $ delete emailTbl $
  (\task -> _sendEmailTask_id task ==. (val_ emailTaskId))

getUnsentEmails
  :: Database Postgres db
  => PgTable Postgres db SendEmailTask
  -> Pg [SendEmailTask Identity]
getUnsentEmails = runSelectReturningList . select . all_
