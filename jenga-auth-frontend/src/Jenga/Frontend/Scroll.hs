module Jenga.Frontend.Scroll where

import Jenga.Frontend.JS
import Jenga.Frontend.DomExtras

import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Language.Javascript.JSaddle
import qualified GHCJS.DOM.Types as DOM
import Control.Lens ((^.))
import Control.Monad
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Text as T

-- | Perhaps this should be routeLinkLocalHash since if we directly request a hash inside a
-- | container on a new page the fragment is ignored anyways
routeLinkHash
  :: forall t m a route.
     ( DomBuilder t m
     , RouteToUrl route m
     , SetRoute t route m
     , Prerender t m
     , PostBuild t m
     )
  => route -- ^ Target route
  -> Maybe T.Text -- ^ Fragment container if we have one
  -> T.Text -- ^ Target Fragment ID
  -> m a -- ^ Child widget
  -> m a
routeLinkHash route mFragContainer frag wrappedChild = do
  (_e, a) <- routeLinkImpl mempty route wrappedChild
  --getPostBuild
  scrollToFrag mFragContainer frag _e
  return a

scrollToTop :: MonadJSM m => m ()
scrollToTop = void $ liftJSM $ jsg ("window") ^. js2 ("scrollTo") (0 :: Int) (0 :: Int)

scrollElem :: MonadJSM m => JSVal -> (Float,Float) -> m ()
scrollElem e (x_,y_) = void $ liftJSM $ e ^. js1 ("scrollBy") opts
  where opts = do
          o <- create
          o ^. jss ("top") y_
          o ^. jss ("left") x_
          o ^. jss ("behavior") ("smooth")
          pure o

getDimensions :: MonadJSM m => T.Text -> m (Float, Float)
getDimensions idTag = liftJSM $ do
  doc <- jsg ("document" )
  idEl <- doc ^. js1 ("getElementById" ) idTag
  width_ <- fromJSVal =<< idEl ^. js ("clientWidth")
  height_ <- fromJSVal =<< idEl ^. js ("clientHeight")
  clog (width_,height_)
  pure (fromMaybe 500 width_, fromMaybe 500 height_)

scrollToId :: MonadJSM m => Maybe T.Text -> T.Text -> m ()
scrollToId containerId idFrag = liftJSM $ do
  w' <- Just <$> currentWindowUnchecked
  doc' <- DOM.unDocument <$> currentDocumentUnchecked
  containerRef <- case containerId of
    Nothing -> pure Nothing
    Just idC -> do
      cont <- doc' ^. js1 ("getElementById") idC
      ghcjsPure (isNull cont) >>= \case
        True -> pure Nothing
        False -> pure $ Just cont

  clog (doc', containerRef)
  --clog (w', doc')
  case (,,) <$> (containerRef <|> (Just doc')) <*> Just doc' <*> w' of
    Nothing -> pure ()
    Just (container, doc, window) -> do
      eWithId <- doc ^. js1 ("getElementById") idFrag


      clog =<< eWithId ^. js0 ("getBoundingClientRect")
      targetY :: Maybe Int <- fromJSVal =<< (eWithId ^. js0 ("getBoundingClientRect") ^. js ("top"))
      containerY :: Maybe Int <- fromJSVal =<< (container ^. js0 ("getBoundingClientRect") ^. js ("top"))

      scrollY' :: Maybe Int <- fromJSVal =<< window ^. js ("scrollY")
      containerScrollTop :: Maybe Int <- fromJSVal =<< container ^. js ("scrollTop")
      clog ("scroll top", containerScrollTop)
      -- const container = document.getElementById("tutorialShell");
      -- const distanceFromTopOfPage = container.getBoundingClientRect().top + window.scrollY;


      case (,,,) <$> targetY <*> containerY <*> containerScrollTop <*> scrollY' of
        Nothing -> pure ()
        Just (tTop, cTop, cScrollTop, scrollY) -> do
          clog (tTop, cTop, cScrollTop)
          --let offset = tTop - cTop
          opts <- do
            o <- create
            o ^. jss ("top") --(offset + cScrollTop)
              (tTop - (cTop + scrollY)) -- cTop)
            -- (fromMaybe 0 $ (+) <$> elemY <*> windowY)
            o ^. jss ("behavior") ("smooth")
            pure o
          _ <- container ^. js1 ("scrollTo") opts
          clog opts
          clog container
          pure ()


scrollToFrag :: forall m t. (Prerender t m, Monad m) => Maybe T.Text -> T.Text -> Event t () -> m ()
scrollToFrag mContainer fragId e = prerender_ blank $ performEvent_ $ ffor e $ \_ -> scrollToId mContainer fragId -- liftJSM $ DOM.currentWindow >>= \case
  -- Nothing -> pure ()
  -- Just win -> scrollToId fragId
