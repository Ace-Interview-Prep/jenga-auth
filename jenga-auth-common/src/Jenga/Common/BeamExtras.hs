module Jenga.Common.BeamExtras where

import Database.Beam.Schema
import Data.Functor.Identity
import Rhyolite.Account
import Data.Signed

type Id a = PrimaryKey a Identity

type MaybeId a = PrimaryKey a (Nullable Identity)

type AuthToken = Signed (PrimaryKey Account Identity)
