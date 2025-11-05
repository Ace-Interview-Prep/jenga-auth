{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Auth.Subscribe.Paywall where

--import Common.Route

import Jenga.Common.Auth
import Jenga.Common.Errors
import Jenga.Common.Stripe
import Obelisk.Route.Frontend
import Reflex.Dom.Core

--import Common.Request
import Rhyolite.Api (ApiRequest(..))
import Control.Monad.IO.Class
import Data.Functor.Identity
import qualified Data.Text as T
import Text.Read



--import Frontend.Auth.Subscribe.StartFreeTrial (freeTrialShell)

data PaywallData t = PaywallData
  { _paywallData_firstName :: Dynamic t T.Text
  , _paywallData_lastName  :: Dynamic t T.Text
  , _paywallData_cardNumber :: Dynamic t T.Text
  , _paywallData_expiryMonth :: Dynamic t T.Text
  , _paywallData_expiryYear :: Dynamic t T.Text
  , _paywallData_cvc :: Dynamic t T.Text
  , _paywallData_click :: Event t ()
  }

-- code userMessage errorEv
data PaywallConfig t = PaywallConfig
  { _paywallConfig_code :: Dynamic t (Maybe T.Text)
  , _paywallConfig_userMessage :: Dynamic t T.Text
  , _paywallConfig_errors :: Event t FrontendError
  }
--(Dynamic t (Maybe T.Text), Dynamic t T.Text)
paywall_FRP
  :: ( Requester t m
     , Request m ~ ApiRequest () publicRequest privateRequest
     , Response m ~ Identity
     --, HeaderConstraints t m
     , PostBuild t m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , SetRoute t (R frontendRoute) m
     , Response m ~ Identity
     )
  => R frontendRoute
  -> ApiRequest () publicRequest privateRequest (Either (BackendError NoFreeTrialCode) (T.Text, Maybe T.Text))
  -> (PaymentFormPrivate -> ApiRequest () publicRequest privateRequest (Either (BackendError SubscribeError) Bool))
  -> PaywallData t
  -> m (PaywallConfig t) --(Event t FrontendError)
paywall_FRP loginRoute mkAPI_hasFreeTrialCode mkAPI_upgradeToPaid (PaywallData _firstName _lastName cardNumber expiryMonth expiryYear cvc click) = mdo
  -- <- paywall_TMPL (code, userMessage)
  pb <- getPostBuild
  let reqCode = ffor pb $ \_ -> mkAPI_hasFreeTrialCode --ApiRequest_Private () $ PrivateRequest_AskFreeTrialCode
  (errFT, email_code :: Event t (T.Text, Maybe T.Text)) <- fmap fanEither $ ( requestingIdentity reqCode  )

  let code' = snd <$> email_code
  code <- holdDyn Nothing code'
  let (validEmailAndNumericals) =
        tag (current $ (,,,,)
             <$> ((readEither . T.unpack :: T.Text -> Either String Int) <$> expiryMonth)
             <*> ((readEither . T.unpack :: T.Text -> Either String Int) <$> expiryYear)
             <*> cardNumber
             <*> cvc
             <*> code
            ) click
  let
    validate :: ( Either String Int
                , Either String Int
                , T.Text
                , T.Text
                , Maybe T.Text
                ) -> Either String PaymentFormPrivate
    validate (expiryMonth', expiryYear', cardNumber', cvc', discountCode') =
      case expiryMonth' of
        Left _ -> Left "Invalid Expiry Month"
        Right month -> case expiryYear' of
          Left _ -> Left "Invalid Expiry Year"
          Right year -> Right $ PaymentFormPrivate cardNumber' month year cvc' discountCode'

  -- we should (maybe) sign this for security like the tokens (may be nicer to do as fields of payment form)
  -- it may also be redundant since the key would need to be shared (i think)
  let (err, paymentFormGood) = fanEither $ validate <$> validEmailAndNumericals
  let req = ffor paymentFormGood $ mkAPI_upgradeToPaid -- \pf -> ApiRequest_Private () $ PrivateRequest_UpgradeToPaid pf
  (errRes, good) <- fmap fanEither $ ( requestingIdentity req  )

  let
    onGood = ffor good $ \case
      True -> "Thank you for subscribing to Ace! Check your email to complete onboarding now and get ready to start interviewing.";
      False -> "Success, Account Re-activated, redirecting now.."
    paymentFormGood' = "Hang tight, we are confirming your subscription..." <$ paymentFormGood

  let
    errorEv = leftmost
      [ Req . Request_ErrorAPI <$> errRes
      , UserError . T.pack <$> err
      , Req . Request_ErrorAPI <$> errFT
      ]
  userMessage <- holdDyn "" $ leftmost [ showUser <$> errorEv, paymentFormGood', onGood ]
  redirect <- delay 2 (ffilter (== False) good)
  setRoute $ loginRoute <$ (ffilter (== False) redirect)
  --pure errorEv

  pure $ PaywallConfig code userMessage errorEv --  paywall_TMPL (code, userMessage)
