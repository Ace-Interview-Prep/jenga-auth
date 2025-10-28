module Jenga.Backend.Handlers.Auth.Subscriptions.IsFreeTrialExpired where

import Jenga.Backend.DB.Subscriptions
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Common.Schema
--import Common.Constants (freeTrialLength, freeDiscountCode)
-- import Common.Types
import Jenga.Common.Errors
import Jenga.Common.BeamExtras
import Rhyolite.Account

import Database.Beam.Postgres
import Database.Beam.Schema

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Pool
import Data.Time.Clock
import Data.Functor.Identity

isFreeTrialExpiredHandler
  :: forall db cfg m.
     ( MonadIO m
     , Database Postgres db
     , HasJengaTable Postgres db FreeTrial
     , HasConfig cfg FreeTrialInfo
     , HasConfig cfg (Pool Connection)
     )
  => Id Account
  -> ReaderT cfg m (Either (BackendError ()) Bool)
isFreeTrialExpiredHandler acctID = do
  freeTrialInfo <- asksM
  (freeTrialTbl :: PgTable Postgres db FreeTrial) <- asksTableM
  (withDbEnv $ lookupFreeTrial freeTrialTbl acctID) >>= \case
    Nothing -> pure $ Right False
    Just freeTrial -> do
      case (== getFreeTrialCode freeTrialInfo) <$> _freeTrial_discountCode freeTrial of
        Just True -> pure $ Right False
        _ -> do
          tNow <- liftIO $ getCurrentTime
          pure . Right $ isExpired tNow freeTrial freeTrialInfo

isExpired :: UTCTime -> FreeTrial Identity -> FreeTrialInfo -> Bool
isExpired tNow freeTrial freeTrialInfo =
  diffUTCTime tNow (_freeTrial_startDate freeTrial) > (getFreeTrialLength freeTrialInfo)
