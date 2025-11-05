{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Auth.Subscribe.StartFreeTrial  where


-- import Frontend.Types
import Jenga.Common.Auth
import Jenga.Common.Errors

import Obelisk.Route.Frontend
import Reflex.Dom.Core

import qualified Text.Email.Validate as EmailValidate
import Control.Monad (join)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Either (isRight)

import Templates.Types
-- import Common.Request
import Rhyolite.Api (ApiRequest(..))
import Data.Functor.Identity

--(email, confirm, clickSubscribe) <- startFreeTrial_TMPL (code, errors)
data StartFreeTrialData t = StartFreeTrialData
  { _startFreeTrialData_email :: Dynamic t T.Text
  , _startFreeTrialData_confirm :: Dynamic t T.Text
  , _startFreeTrialData_clickSubscribe :: Event t ()
  }
--(Dynamic t T.Text, Dynamic t T.Text, Event t ())
data StartFreeTrialConfig t = StartFreeTrialConfig
  { _startFreeTrialConfig_code :: Dynamic t (Maybe T.Text)
  , _startFreeTrialConfig_errors :: Dynamic t T.Text
  , _startFreeTrialConfig_success :: Event t ()
  }

startFreeTrial_FRP
  :: ( Routed t (Map.Map T.Text (Maybe T.Text)) m
     , Requester t m
     , Request m ~ ApiRequest token publicRequest privateRequest
     , Response m ~ Identity
     , Template t m
     )
  => ((Maybe T.Text, Email) -> ApiRequest token publicRequest privateRequest (Either (BackendError FreeTrialError) ()))
  -> StartFreeTrialData t
  -> m (StartFreeTrialConfig t)
startFreeTrial_FRP mkAPI_NewFreeTrial (StartFreeTrialData email confirm clickSubscribe) = mdo
  --(email, confirm, clickSubscribe) <- startFreeTrial_TMPL (code, errors)
  queryParams :: Dynamic t (Map.Map T.Text (Maybe T.Text)) <- askRoute
  let code = join . Map.lookup "code" <$> queryParams
  let
    confirmedEmailF e conf =
      if e /= conf
      then Left "Emails don't match"
      else
        if (isRight $ EmailValidate.validate . T.encodeUtf8 $ e)
        then Right $ Email e
        else Left "Invalid Email"
    (emailErr, emailsGood) = fanEither $ tag (current $ confirmedEmailF <$> email <*> confirm) clickSubscribe
  let req = ffor (attach (current code) emailsGood) $ mkAPI_NewFreeTrial

  (errRes, good) <- fmap fanEither $ ( requestingIdentity req  )
  errors <- holdDyn "" $ leftmost
    [ showUser <$> errRes
    , emailErr
    ]
  pure $ StartFreeTrialConfig code errors good
