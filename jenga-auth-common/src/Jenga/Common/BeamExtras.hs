module Jenga.Common.BeamExtras where

import Database.Beam.Schema
import Data.Functor.Identity

type Id a = PrimaryKey a Identity

type MaybeId a = PrimaryKey a (Nullable Identity)
