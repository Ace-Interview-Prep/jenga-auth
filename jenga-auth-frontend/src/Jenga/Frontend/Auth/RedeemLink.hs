{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Auth.RedeemLink where

import Templates.Types
import Jenga.Common.Errors
import Jenga.Common.Auth
import Rhyolite.Api (ApiRequest(..))
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Control.Monad.Fix
import qualified Data.Text as T
import Data.Functor.Identity


newtype RedeemLinkConfig t = RedeemLinkConfig
  { _redeemLinkConfig_errors :: Dynamic t (Maybe T.Text)
  }

data RedeemLinkData t m = RedeemLinkData
  { _redeemLink_email :: InputEl t m
  , _redeemLink_confirmEmail :: InputEl t m
  , _redeemLink_submit :: Event t ()
  }

redeemLink_FRP
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Routed t T.Text m
     , Requester t m
     , Request m ~ ApiRequest token publicRequest privateRequest
     , Response m ~ Identity
     )
  => ((T.Text, Email) -> ApiRequest token publicRequest privateRequest (Either (BackendError RedeemLinkError) ()))
  -> RedeemLinkData t m
  -> m (RedeemLinkConfig t)
redeemLink_FRP mkAPI (RedeemLinkData email' eConfirm' submit) = mdo
  --RedeemLinkData email' eConfirm' submit <- redeemLink_TMPL $ RedeemLinkConfig errors
  let email = value email'
  let eConfirm = value eConfirm'
  let
    checkMatch = (\(e,confirm) ->
                    if e == confirm
                    then Right e
                    else Left "Emails dont match"
                 )
    emails_agree = fmap checkMatch $ (,) <$> email <*> eConfirm
  let (bad, good) = fanEither (tag (current emails_agree) submit)
  codeLink <- askRoute
  let req = ffor (attach (current codeLink) $ Email <$> good) $ mkAPI
        -- \(code, email_) ->
        -- ApiRequest_Public $ PublicRequest_RedeemLink code email_
  (err, res) <- fmap fanEither $ ( requestingIdentity req  )


  let errorsEv = Just <$> leftmost [ showUser <$> err
                                   , bad
                                   , "Success, please check your email" <$ res
                                   ]
  errors <- holdDyn Nothing errorsEv
  pure $ RedeemLinkConfig errors
