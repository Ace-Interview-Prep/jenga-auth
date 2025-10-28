{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Jenga.Common.Errors where

import Database.Beam
import qualified Data.Text as T
import GHC.TypeLits
import Data.Aeson



class ShowUser a where
  showUser :: a -> T.Text

instance ShowUser () where
  showUser _ = "Empty Error"

--- Various backend errors
class SpecificError a

instance (TypeError ('Text "Instances of IsString are not allowed, please use a handler-specific enum")) => SpecificError (BackendError String)
instance (TypeError ('Text "Instances of IsString are not allowed, please use a handler-specific enum")) => SpecificError (BackendError T.Text)



-- | The forall denotes a 'skolem' type, which ultimately just means
-- | we are using ExistentialQuantification in order to allow e to be many different things
-- | but in a way that we can operate as if type a and type b are just one type.
-- | The one downside is that it is impossible to pattern match to discover what type 'err' is
-- | but we do have the functionality that the classes here give us.
-- | This type is simply to collect errors to notify us of any that happen. If we need to pattern match
-- | on err then we can simply use it the value before we wrap it in Req
data FrontendError
  = forall err. ErrorRequirements err => Req (RequestError err)
  | UserError T.Text
  | JSError T.Text
  | Unexpected T.Text
type ErrorRequirements err = (Show err, ShowUser err, ToJSON err, FromJSON err, Typeable err)

req_ :: RequestError () -> FrontendError
req_ = Req

data RequestError e
  = Request_ErrorRead T.Text
  -- ^ Invalid JSON or different XhrResponseBody than expected
  | Request_ErrorAPI (BackendError e)
  | Request_ErrorTimeout T.Text
  deriving (Show, Eq)

-- data BackendError e = BackendError e
--instance Show e => Show (BackendError e) where

-- TODO: move to backend when we can -> a BackendError should be unwrapped and
-- handled before reaching the frontend
-- data BackendError a where
--   BUserError :: a -> BackendError a -- this can happen
--   BCritical :: a -> BackendError a  -- this should never happen
--   BNoAuth :: BackendError AuthError

data BackendError e
  = NoAuth
  | NoAuth_NoCookie
  | NoAuth_CantReadKey
  | NoAuth_NoUserTypeCookie
  | BUserError e
  | BCritical e
  | BException T.Text
  | BInvalidRequest T.Text
  -- ^ For use with raw HTTP APIs. e var allows for dumping arbitrary info
  deriving (Eq, Generic)

instance ToJSON e => ToJSON (BackendError e)
instance FromJSON e => FromJSON (BackendError e)

instance Functor BackendError where
  fmap _ NoAuth = NoAuth
  fmap _ NoAuth_NoCookie = NoAuth_NoCookie
  fmap _ NoAuth_CantReadKey = NoAuth_CantReadKey
  fmap _ NoAuth_NoUserTypeCookie = NoAuth_NoUserTypeCookie
  fmap f (BUserError e) = BUserError (f e)
  fmap f (BCritical e) = BCritical (f e)
  fmap _ (BException s) = BException s
  fmap _ (BInvalidRequest e) = BInvalidRequest e
--- data AuthError = NoAuth deriving Show

instance ShowUser a => ShowUser (BackendError a) where
  showUser NoAuth = "No Auth Token found"
  showUser NoAuth_NoCookie = "No Auth Cookie Found"
  showUser NoAuth_CantReadKey = "Failed to serialize key"
  showUser NoAuth_NoUserTypeCookie = "No user type found on dependent request"
  showUser (BUserError e) = showUser e
  showUser (BCritical e) = "Something has gone wrong with your request and has been automatically reported, we apologize for the inconvenience ... ERROR=" <> showUser e
  showUser (BException e) = "Something has gone wrong with your request and has been automatically reported, we apologize for the inconvenience ... ERROR=" <> e
  showUser (BInvalidRequest e) = "Invalid HTTP Request Body and/or Headers: " <> e

deriving instance Show e => Show (BackendError e)

instance ShowUser e => ShowUser (RequestError e) where
  showUser (Request_ErrorRead msg) = msg
  showUser (Request_ErrorAPI msg) = showUser msg
  showUser (Request_ErrorTimeout msg) = msg

instance ShowUser FrontendError where
  showUser = \case
    Req r -> showUser r
    UserError msg -> msg
    Unexpected msg -> msg
    JSError msg -> msg



-- Old
type ApiError = T.Text
type ErrorRead = String
type ApiResponse a = Either ErrorRead (Either ApiError a)
type ApiBEResponse e a = Either ErrorRead (Either (BackendError e) a)
