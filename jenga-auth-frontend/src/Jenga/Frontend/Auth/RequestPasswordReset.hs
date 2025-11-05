{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Auth.RequestPasswordReset where --ResetPasswordAce where

import Templates.Types
import Jenga.Common.Errors
import Jenga.Common.Auth

import Rhyolite.Api (ApiRequest(..))
import Reflex.Dom.Core

import Control.Monad.Fix
import Data.Functor.Identity
import qualified Data.Text as T

newtype EmailForPasswordResetConfig t = EmailForPasswordResetConfig
  { _emailForPasswordResetConfig_errors :: Dynamic t (Maybe T.Text)
  }

data EmailForPasswordResetData t m = EmailForPasswordResetData
  { _emailForPasswordReset_email :: InputEl t m
  , _emailForPasswordReset_confirmEmail :: InputEl t m
  , _emailForPasswordReset_submit :: Event t ()
  }

-- primaryButton :: DomBuilder t m => T.Text -> m (Event t ())
-- primaryButton buttonText = do
--   (e, _) <- elClass' "button" classes $ text buttonText
--   pure $ domEvent Click e
--   where
--     classes =
--       "focus:outline-none w-full p-4 mt-16 shadow-button bg-primary \
--       \ font-facit font-bold text-white text-body text-center rounded \
--       \ hover:bg-primary-rich active:bg-primary-desaturated \
--       \ focus:ring-4 ring-primary ring-opacity-50"

-- requestPasswordReset :: ( DomBuilder t m
--                         , PostBuild t m
--                         , MonadFix m
--                         , MonadHold t m
--                         , Request m ~ ApiRequest token PublicRequest PrivateRequest
--                         , Response m ~ Identity
--                         , Requester t m
--                         ) => m ()
-- requestPasswordReset = requestPasswordReset_FRP

-- requestPasswordReset_TMPL :: ( DomBuilder t m
--                              , PostBuild t m
--                              , MonadFix m
--                              , MonadHold t m
--                              )
--                           => EmailForPasswordResetConfig t
--                           -> m (EmailForPasswordResetData t m)
-- requestPasswordReset_TMPL cfg = do
--   authFormTemplate hectorRecommendation $ do
--     authFormTitle "Recover Password"
--     email <- authFormRow $ do
--       authFormLabel "Email"
--       authFormTextInput "email" "Enter your email"
--     emailConfirm <- authFormRow $ do
--       authFormLabel "Confirm your email"
--       authFormTextInput "email" "you@example.com"

--     maybeDisplay errorMessage $ _emailForPasswordResetConfig_errors cfg
--     submit <- primaryButton "Reset Password"

--     pure $ EmailForPasswordResetData email emailConfirm submit

requestPasswordReset_FRP
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Request m ~ ApiRequest token publicRequest privateRequest
     , Response m ~ Identity
     , Requester t m
     )
  => (Email -> ApiRequest token publicRequest privateRequest
      (Either (BackendError RequestPasswordResetError) ())
     )
  -> (EmailForPasswordResetData t m)
  -> m (EmailForPasswordResetConfig t)
requestPasswordReset_FRP mkAPI (EmailForPasswordResetData email' eConfirm' submit) = mdo
  --EmailForPasswordResetData email' eConfirm' submit <- requestPasswordReset_TMPL $ EmailForPasswordResetConfig errors
  let email = value email'
  let eConfirm = value eConfirm'
  let
    f = (\(a,b) ->
            if a == b
            then (Right a)
            else Left "Emails dont match"
        )
    emails_agree = fmap f $ (,) <$> email <*> eConfirm
  let (bad, good) = fanEither (tag (current emails_agree) submit)
  res <- requestingIdentity $ ffor good $ \goodEmail -> mkAPI $ Email goodEmail
  let (apiError, goodResponse) = fanEither res
  let errorsEv = Just <$> leftmost
        [ bad
        , showUser <$> apiError
        , "Success, please check your email" <$ goodResponse
        ]
  errors <- holdDyn Nothing errorsEv
  pure $ EmailForPasswordResetConfig errors
