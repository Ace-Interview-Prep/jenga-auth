{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Jenga.Frontend.Auth.Signup where

--import Jenga.Frontend.MkTmpl
import Jenga.Common.Auth
import Templates.Partials.Checkbox
import Templates.Types
import Jenga.Common.Errors

import Rhyolite.Api (ApiRequest(..))
import Obelisk.Route.Frontend
import Reflex
import Reflex.Dom.Core hiding (checkbox, Checkbox(..), CheckboxConfig(..))

import Control.Monad.Fix
import qualified Text.Email.Validate as EmailValidate
import Data.Functor.Identity
import qualified Data.Text.Encoding as T
import qualified Data.Text as T


data SignupConfig t = SignupConfig
  { _signupConfig_errors :: Dynamic t (Maybe T.Text)
  , _signupConfig_success :: Event t ()
  }


data SignupData t m = SignupData
  { _signup_email :: InputEl t m
  , _signup_confirmEmail :: InputEl t m
  , _signup_agreeToTerms :: Checkbox t
  , _signup_submit :: Event t ()
  }


-- | TODO(anyone): It would be really cool to populate forms with info they've already entered so that they can move forward faste
-- | we could do this through query params
newSignup_FRP
  :: ( DomBuilder t m
     , SetRoute t (R frontendRoute) m
     , RouteToUrl (R frontendRoute) m
     , Prerender t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Requester t m
     , Request m ~ ApiRequest token publicRequest privateRequest
     , Response m ~ Identity
     )
  => (Email -> ApiRequest token publicRequest privateRequest
      (Either (BackendError UserSignupError) ())
     )
  --forall app. NewCompanyEmail -> PublicApi app (Either e a)
  -> (SignupData t m)
  -> m (SignupConfig t) --(Event t ()) --()
newSignup_FRP mkAPI (SignupData email' eConfirm' agree' submit) = mdo
  --Signup email' eConfirm' orgName' agree' submit <- newCompanySignup_TMPL $ SignupConfig errors
  let email = value email'
  let eConfirm = value eConfirm'
  let agree = value agree'
  let
    validate = (\(emailA,emailB,agree'') ->
           if agree'' == False
           then Left "You must accept terms"
           else
             if emailA /= emailB
             then Left "Emails dont match"
             else
               case EmailValidate.validate $ T.encodeUtf8 emailA of
                     Left _ -> Left "Invalid Email"
                     Right _ ->
                       Right $ Email emailA
               )
    valid = fmap validate $ (,,) <$> email <*> eConfirm <*> agree
  let (bad, good') = fanEither (tag (current valid) submit)
  let request = ffor good' $ mkAPI -- ApiRequest_Public . PublicRequest_CompanySignup
  response <- requestingIdentity request
  let (errRes, goodResponse) = fanEither response
  let errorsEv = Just <$> leftmost
        [ bad
        , showUser . Req . Request_ErrorAPI <$> errRes
        , "Success, please check your email" <$ goodResponse
        ]
  errors <- holdDyn Nothing errorsEv

  pure $ SignupConfig errors goodResponse -- (signupgoodResponse
