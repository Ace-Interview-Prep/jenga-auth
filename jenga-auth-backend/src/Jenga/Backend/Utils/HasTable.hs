module Jenga.Backend.Utils.HasTable where

import Jenga.Backend.Utils.Query
import Jenga.Backend.Utils.HasConfig
import Database.Beam.Postgres
import Database.Beam.Schema
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Pool


class HasJengaTable dbHost db tbl where
  tableRef :: PgTable dbHost db tbl

type PgTable dbHost db x = DatabaseEntity dbHost db (TableEntity x)

asksTableM
  :: (HasJengaTable dbHost db tbl, Monad m)
  => ReaderT cfg m (PgTable dbHost db tbl)
asksTableM = do
  pure tableRef

withDbEnv :: (HasConfig cfg (Pool Connection), MonadIO m) => Pg a -> ReaderT cfg m a
withDbEnv query_ = flip runSerializable query_ =<< asksM

withDbEnvQuiet  :: (HasConfig cfg (Pool Connection), MonadIO m) => Pg a -> ReaderT cfg m a
withDbEnvQuiet query = do
  dbConn <- asksM
  runDbQuiet dbConn query
