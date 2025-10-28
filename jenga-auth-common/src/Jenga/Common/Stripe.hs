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
module Jenga.Common.Stripe where

import Jenga.Common.ScrubPrefix
import Web.Stripe.Error
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import qualified Data.Text as T


instance ToJSON StripeError
deriving instance Generic StripeError
instance ToJSON StripeErrorHTTPCode
instance ToJSON StripeErrorCode
instance ToJSON StripeErrorType
deriving instance Generic StripeErrorHTTPCode
deriving instance Generic StripeErrorCode
deriving instance Generic StripeErrorType


-- | Private means authenticated
data PaymentFormPrivate = PaymentFormPrivate
  { _paymentForm_cardNumber :: T.Text
  , _paymentForm_expiryMonth :: Int
  , _paymentForm_expiryYear :: Int
  , _paymentForm_cvc :: T.Text
  , _paymentForm_discountCode :: Maybe T.Text
  } deriving Generic

deriveJSON (scrubPrefix "_paymentForm_") ''PaymentFormPrivate
