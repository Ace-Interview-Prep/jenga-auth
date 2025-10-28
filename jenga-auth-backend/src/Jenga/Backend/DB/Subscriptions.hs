module Jenga.Backend.DB.Subscriptions where

-- import Backend.DB (db)
-- import Common.Schema
-- import Common.Types
import Jenga.Backend.DB.Auth
import Jenga.Backend.Utils.HasTable
import Jenga.Common.BeamExtras
import Jenga.Common.Schema

import Database.Beam.Backend.SQL.Types
import Rhyolite.Account
import Web.Stripe.Subscription
import Database.Beam.Schema
import Database.Beam.Postgres
import Database.Beam.Query

import Control.Monad.IO.Class
import Data.Functor.Identity
import qualified Data.Text as T
import Data.Time.Clock

lookupFreeTrial :: Database Postgres db => PgTable Postgres db FreeTrial -> Id Account -> Pg (Maybe (FreeTrial Identity))
lookupFreeTrial freeTrialTbl aid = runSelectReturningOne $ select $ do
  freeTrials <- all_ freeTrialTbl
  guard_ $ _freeTrial_userID freeTrials ==. (val_ $ acctIDtoInt64 aid)
  pure freeTrials

putNewFreeTrial :: Database Postgres db => PgTable Postgres db FreeTrial -> Id Account -> Maybe T.Text -> Pg ()
putNewFreeTrial freeTrialTbl aid mCode = do
  utc <- liftIO $ getCurrentTime
  runInsert $ insert freeTrialTbl $ insertExpressions
    [ FreeTrial (val_ $ acctIDtoInt64 aid) (val_ utc) (val_ mCode) ]

putNewStripeInfo :: Database Postgres db => PgTable Postgres db StripeRelation -> Id Account -> CustomerId -> SubscriptionId -> Pg ()
putNewStripeInfo stripeTbl aid (CustomerId cid) (SubscriptionId sid) = runInsert $ insert stripeTbl $ insertExpressions
  [ StripeRelation (val_ $ unSerial . _accountId_id $ aid) (val_ cid) (val_ $ Just sid)
  ]

getStripeInfo :: Database Postgres db => PgTable Postgres db StripeRelation -> Id Account -> Pg (Maybe (StripeRelation Identity))
getStripeInfo stripeTbl aid = runSelectReturningOne $ select $ do
  infos <- all_ stripeTbl
  guard_ $ _stripeRelation_uid infos ==. (val_ $ acctIDtoInt64 aid)
  pure infos

recordEndSubscription :: Database Postgres db => PgTable Postgres db StripeRelation -> Id Account -> Pg ()
recordEndSubscription stripeTbl aid = runUpdate $
  update
  stripeTbl
  (\stripeInfo -> _stripeRelation_subscriptionId stripeInfo <-. val_ Nothing)
  (\stripeInfo -> _stripeRelation_uid stripeInfo ==. (val_ $ acctIDtoInt64 aid))

isSubscriber :: Database Postgres db => PgTable Postgres db StripeRelation -> Id Account -> Pg Bool
isSubscriber stripeTbl aid = do
  x <- runSelectReturningOne $ select $ do
    filter_ (\stripeR -> _stripeRelation_uid stripeR ==. (val_ . acctIDtoInt64 $ aid)) $ all_ stripeTbl
  case x of
    Nothing -> pure False
    Just _ -> pure True

getUsersDiscountCode :: Database Postgres db => PgTable Postgres db FreeTrial -> Id Account -> Pg (Maybe (Maybe DiscountCode))
getUsersDiscountCode freeTrialTbl aid = fmap (fmap _freeTrial_discountCode ) $ runSelectReturningOne $ select $ do
  fTrials <- all_ freeTrialTbl
  guard_ $ _freeTrial_userID fTrials ==. (val_ $ acctIDtoInt64 aid)
  pure fTrials

-- |todo: figure out how to do this in one shot not two
-- | and should we be making a new customer id?
recordResubscribe :: Database Postgres db => PgTable Postgres db StripeRelation -> Id Account -> CustomerId -> SubscriptionId -> Pg ()
recordResubscribe stripeTbl aid (CustomerId cid) (SubscriptionId sid) = do
  runUpdate $ update
    stripeTbl
    (\stripeInfo -> _stripeRelation_subscriptionId stripeInfo <-. val_ (Just sid))
    (\stripeInfo -> _stripeRelation_uid stripeInfo ==. (val_ $ acctIDtoInt64 aid))
  runUpdate $ update
    stripeTbl
    (\stripeInfo -> _stripeRelation_customerId stripeInfo <-. val_ (cid))
    (\stripeInfo -> _stripeRelation_uid stripeInfo ==. (val_ $ acctIDtoInt64 aid))
