{-# OPTIONS_GHC -fno-warn-orphans #-}
module Jenga.Backend.DB.Instances where


-- Aggregate orphan instances
import Jenga.Backend.DB.BeamInstances ()
import Rhyolite.Backend.Account ()
import Network.Mail.Mime.Orphans ()

import Jenga.Common.Auth
import Rhyolite.Account

import Database.Beam.AutoMigrate.Compat
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.Beam.Backend.SQL.BeamExtensions

import Network.Mail.Mime (Mail)
import Data.Proxy
import Data.Int (Int64)
import qualified Data.Text as T
-- TODO: why is this needed?

instance FromField (SqlSerial Int64) where
  fromField f mbs = fmap SqlSerial $ fromField f mbs
instance ToField a => ToField (SqlSerial a) where
  toField (SqlSerial a) = toField a
instance FromField (PrimaryKey Account Identity) where
  fromField f mbs = AccountId <$> fromField f mbs

instance ToField (PrimaryKey Account Identity) where
  toField (AccountId x) = toField x


instance HasColumnType Mail where
  defaultColumnType _ = defaultColumnType $ Proxy @(PgJSON Mail)

instance HasSqlValueSyntax PgValueSyntax Mail where
  sqlValueSyntax mail = sqlValueSyntax $ PgJSON mail

instance FromBackendRow Postgres Mail

instance FromField Mail where
  fromField f mbs = (\(PgJSON mail) -> mail) <$> fromField f mbs

instance HasSqlValueSyntax PgValueSyntax UserType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres UserType

instance FromField UserType where
  fromField f d = read <$> fromField f d

instance HasColumnType UserType where
  defaultColumnType _ = defaultColumnType $ Proxy @T.Text

instance HasSqlEqualityCheck Postgres UserType
