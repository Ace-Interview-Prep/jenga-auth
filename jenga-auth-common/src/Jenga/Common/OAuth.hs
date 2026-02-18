{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Jenga.Common.OAuth where

import Data.Aeson
import Data.Text (Text)
import Data.Int (Int64)
import Data.Kind (Constraint)
import GHC.Generics
import Database.Beam

-- | Google OAuth ID table
data GoogleID f = GoogleID
  { _googleID_gid :: Columnar f Text  -- Google user ID is a string
  , _googleID_userID :: Columnar f Int64
  } deriving Generic

instance Beamable GoogleID

instance Table GoogleID where
  newtype PrimaryKey GoogleID f = GooglePK { unGoogleIDId :: Columnar f Text }
    deriving Generic
  primaryKey = GooglePK <$> _googleID_gid

type HasGoogleIDConstraint (c :: * -> Constraint) f =
  ( c (Columnar f Text)
  , c (Columnar f Int64)
  )

deriving instance HasGoogleIDConstraint Eq f => Eq (GoogleID f)
deriving instance HasGoogleIDConstraint Ord f => Ord (GoogleID f)
deriving instance HasGoogleIDConstraint Show f => Show (GoogleID f)

instance Beamable (PrimaryKey GoogleID)

-- | Discord OAuth ID table
data DiscordID f = DiscordID
  { _discordID_did :: Columnar f Text  -- Discord user ID (snowflake as string)
  , _discordID_userID :: Columnar f Int64
  } deriving Generic

instance Beamable DiscordID

instance Table DiscordID where
  newtype PrimaryKey DiscordID f = DiscordPK { unDiscordIDId :: Columnar f Text }
    deriving Generic
  primaryKey = DiscordPK <$> _discordID_did

type HasDiscordIDConstraint (c :: * -> Constraint) f =
  ( c (Columnar f Text)
  , c (Columnar f Int64)
  )

deriving instance HasDiscordIDConstraint Eq f => Eq (DiscordID f)
deriving instance HasDiscordIDConstraint Ord f => Ord (DiscordID f)
deriving instance HasDiscordIDConstraint Show f => Show (DiscordID f)

instance Beamable (PrimaryKey DiscordID)

-- | Google user data from OAuth
data GoogleUser = GoogleUser
  { _googleUser_id :: String
  , _googleUser_email :: String
  , _googleUser_verified_email :: Bool
  , _googleUser_name :: Maybe String
  , _googleUser_given_name :: Maybe String
  , _googleUser_family_name :: Maybe String
  , _googleUser_picture :: Maybe String
  , _googleUser_locale :: Maybe String
  } deriving (Show, Generic)

instance FromJSON GoogleUser where
  parseJSON = withObject "GoogleUser" $ \v ->
    GoogleUser
      <$> v .: "id"
      <*> v .: "email"
      <*> v .: "verified_email"
      <*> v .:? "name"
      <*> v .:? "given_name"
      <*> v .:? "family_name"
      <*> v .:? "picture"
      <*> v .:? "locale"

-- | Discord user data from OAuth
data DiscordUser = DiscordUser
  { _discordUser_id :: String
  , _discordUser_username :: String
  , _discordUser_discriminator :: String
  , _discordUser_avatar :: Maybe String
  , _discordUser_email :: Maybe String
  , _discordUser_verified :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON DiscordUser where
  parseJSON = withObject "DiscordUser" $ \v ->
    DiscordUser
      <$> v .: "id"
      <*> v .: "username"
      <*> v .: "discriminator"
      <*> v .:? "avatar"
      <*> v .:? "email"
      <*> v .:? "verified"

-- | GitHub email data from /user/emails endpoint
data GitHubEmail = GitHubEmail
  { _githubEmail_email :: String
  , _githubEmail_primary :: Bool
  , _githubEmail_verified :: Bool
  , _githubEmail_visibility :: Maybe String
  } deriving (Show, Generic)

instance FromJSON GitHubEmail where
  parseJSON = withObject "GitHubEmail" $ \v ->
    GitHubEmail
      <$> v .: "email"
      <*> v .: "primary"
      <*> v .: "verified"
      <*> v .:? "visibility"

-- | OAuth provider type
data OAuthProvider = GitHub | Google | Discord
  deriving (Show, Eq, Generic)
