{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PackageImports #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- for validGregorian / Day instance
module Jenga.Backend.Utils.Date where

import Data.Time
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax
import Control.Monad.IO.Class
import Control.Monad

{- helpers for dates -}
--fromGregorianValid :: Integer -> Int -> Int -> Maybe Day

type GivesQ a = Q Exp

deriving instance Lift Day

-- | Extremely safe date constant constructor, used with date-sensitive migrations
validGregorian :: Integer -> Int -> Int -> GivesQ Day
validGregorian year month day = do
  case fromGregorianValid year month day of
    Nothing -> error "Invalid date"
    Just dayGregorian -> [| dayGregorian |]

-- | This corrects user if they accidentally switch args around
isBetween :: Day -> Day -> Day -> Bool
isBetween start end day = start' <= day && day <= end'
  where
    start' = min start end
    end'   = max start end

isCurrentDayBetween :: Day -> Day -> IO Bool
isCurrentDayBetween start end = isBetween start end . utctDay <$> getCurrentTime

whenInDateRange :: MonadIO m => Day -> Day -> m a -> m (Maybe a)
whenInDateRange start end pgAction = do
  (liftIO $ isCurrentDayBetween start end) >>= \case
    False -> liftIO $ putStrLn "WARNING: stale action skipped" >> pure Nothing
    True  -> Just <$> pgAction

whenInDateRange_ :: MonadIO m => Day -> Day -> m a -> m ()
whenInDateRange_ start end action = do
  (liftIO $ isCurrentDayBetween start end) >>= \case
    False -> liftIO $ putStrLn "WARNING: stale action skipped"
    True  -> void action
