{-
Utils for editing and running migrations
-}
module Jenga.Backend.Utils.Migrations where

import Database.Beam.AutoMigrate

markSafe :: Edit -> Edit
markSafe e = e { _editCondition = Right Safe }

markSafeWhen :: Edit -> Bool -> Edit
markSafeWhen this_ b = if b then markSafe this_ else markUnsafe this_

markUnsafe :: Edit -> Edit
markUnsafe e = e { _editCondition = Right Unsafe }
