module Jenga.Backend.Utils.HasTable where

import Database.Beam.Schema
import Control.Monad.Trans.Reader


class HasJengaTable dbHost db tbl where
  tableRef :: PgTable dbHost db tbl
type PgTable dbHost db x = DatabaseEntity dbHost db (TableEntity x)
asksTableM
  :: (HasJengaTable dbHost db tbl, Monad m)
  => ReaderT cfg m (PgTable dbHost db tbl)
asksTableM = do
  pure tableRef
