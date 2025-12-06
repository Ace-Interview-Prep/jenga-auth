module Jenga.Backend.DB.LogItems where

-- import Backend.DB (db)

-- import Common.Schema
import Jenga.Backend.Utils.HasTable
import Jenga.Common.Schema

import Database.Beam.Schema
import Database.Beam.Query
import Database.Beam.Postgres
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Time.Clock

getUnreportedUrgentLogItems
  :: Database Postgres db
  => PgTable Postgres db LogItemRow
  -> Pg [LogItemRow Identity]
getUnreportedUrgentLogItems tbl = runSelectReturningList $ select $ do
  filter_
    (\logItemR -> _logItemRow_hasBeenSent logItemR ==. (val_ False)
                  &&. _logItemRow_isUrgent logItemR ==. (val_ True)
    ) (all_ tbl)

getNonUrgentLogItems
  :: Database Postgres db
  => PgTable Postgres db LogItemRow
  -> NominalDiffTime
  -> Pg [LogItemRow Identity]
getNonUrgentLogItems tbl since = do
  t <- liftIO getCurrentTime
  let sinceUtc = addUTCTime (- (abs since)) t
  runSelectReturningList $ select $ do
    --all_ (_db_reporting db)
    filter_
      (\logItemR ->
         -- _logItemRow_hasBeenSent logItemR ==. (val_ False)
         -- &&.
         _logItemRow_isUrgent logItemR ==. (val_ False)
         &&.
         _logItemRow_insertionTime logItemR >. (val_ sinceUtc)
      ) (all_ tbl)

markLogSent
  :: Database Postgres db
  => PgTable Postgres db LogItemRow
  -> [LogItemRow Identity]
  -> Pg ()
markLogSent tbl logItems = do
  let ids = _logItemRow_id <$> logItems
  runUpdate $ update
    tbl
    (\logItem -> _logItemRow_hasBeenSent logItem <-. val_ True)
    (\logItem -> in_ (_logItemRow_id logItem) (val_ <$> ids))
