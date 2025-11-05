{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Platform where

import Jenga.Common.Auth
import Jenga.Common.HasJengaConfig
import Obelisk.Configs
import Language.Javascript.JSaddle
import Network.URI as URI
import Control.Lens ((^.))
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding as T

getDomain :: URI -> Maybe T.Text
getDomain baseUri_ =
  (T.pack . URI.uriRegName)
  --- $ fromMaybe (error "no domain we can set cookies for")
  <$> URI.uriAuthority baseUri_

chooseRouteFile :: HasConfigs m => m T.Text
chooseRouteFile = do
  route_ <- getConfig "common/route"
  case parseURI =<< T.unpack . T.strip . T.decodeUtf8 <$> route_ of
    Nothing -> pure "common/route"
    Just uri_ -> case getDomain uri_ of
#ifdef android_HOST_OS
      Just "localhost" -> do
        -- On mobile
        getConfig "common/ngrokRoute" >>= \case
          Nothing -> pure "common/route"
          Just _ -> pure "common/ngrokRoute"
#else
      Just "localhost" -> pure "common/route"
#endif
      _ -> pure "common/route"

--- in Frontend.hs
-- baseUrl <- readJengaBaseUrlFromConfigsFiles
--
-- TODO: can we get even crazier and include configs, validated at TH step?
-----  -> need to ensure no security concerns first

-- data FrontendConfig = FrontendConfig
--   { _baseUrl :: BaseURL
--   }
-- instance HasConfig FrontendConfig BaseURL where
--   fromCfg = _baseUrl


readJengaBaseURL :: HasConfigs m => m (Maybe BaseURL)
readJengaBaseURL = do
  route_ <- getConfig "common/route"
  uriString <- case parseURI =<< T.unpack . T.strip . T.decodeUtf8 <$> route_ of
    Nothing -> pure "https://acetalent.io"
    Just uri_ -> case getDomain uri_ of
#ifdef android_HOST_OS
      Just "localhost" -> do
        -- On mobile development
        getConfig "common/ngrokRoute" >>= \case
          Nothing -> pure . T.pack $ show uri_
          Just url -> pure . T.strip . T.decodeUtf8 $ url
#else
      Just "localhost" -> pure . T.pack $ show uri_
#endif
      _ -> pure . T.pack $ show uri_
  pure $ fmap BaseURL . parseURI . T.unpack $ uriString
isNotMobile :: JSM Bool
isNotMobile = do
  width_ <- fromJSVal =<< (jsg (s "document")) ^. js (s "documentElement") ^. js (s "clientWidth")
  return (fromMaybe 1000 width_ > (640 :: Int))
  where
    s :: String -> String
    s = id

clientType :: ClientType
clientType =
#ifdef android_HOST_OS
  Mobile
#else
  Web
#endif
