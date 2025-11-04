{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.DomExtras where

import Control.Lens ((%~))
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Language.Javascript.JSaddle
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Element as Element

import Control.Monad
import Data.Proxy
import qualified Data.Map as Map
import qualified Data.Text as T

elDynHtmlAttr_
  :: ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , DOM.MonadJSM (Performable m)
     , Element.IsElement (RawElement (DomBuilderSpace m))
     )
  => T.Text
  -> Map.Map T.Text T.Text
  -> Dynamic t T.Text
  -> m (Element EventResult (DomBuilderSpace m) t)
elDynHtmlAttr_ elementTag attrs_ html = do
  let cfg = def & initialAttributes .~ Map.mapKeys (AttributeName Nothing) attrs_
  (e, _) <- element elementTag cfg $ return ()
  postBuild <- getPostBuild
  performEvent_ $ liftJSM . Element.setInnerHTML (_element_raw e) <$> leftmost [updated html, tag (current html) postBuild]
  return e



routeLinkImpl
  :: forall t m a route.
     ( DomBuilder t m
     , RouteToUrl route m
     , SetRoute t route m
     )
  => Map.Map AttributeName T.Text
  -> route -- ^ Target route
  -> m a -- ^ Child widget
  -> m (Event t (), a)
routeLinkImpl attrs route wrappedChild = do
  enc <- askRouteToUrl
  let
    -- If targetBlank == True, the link will be opened in another page. In that
    -- case, we don't prevent the default behaviour, and we don't need to
    -- setRoute.
    targetBlank = Map.lookup "target" attrs == Just "_blank"
    cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_initialAttributes .~ ("href" =: enc route <> attrs)
        & (if targetBlank
           then id
           else elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const preventDefault))
  (e, a) <- element "a" cfg wrappedChild
  when targetBlank $ pure ()
  --when (not targetBlank) $ setRoute $ route <$ domEvent Click e
  return (domEvent Click e, a)
