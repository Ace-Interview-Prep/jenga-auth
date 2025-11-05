module Jenga.Backend.Utils.Snap where

-- import Common.Route
import Jenga.Common.Errors
import Jenga.Backend.Utils.HasConfig

import Obelisk.Route
import Snap
import Snap.Extras (writeJSON)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Applicative
import Data.Aeson as Aeson
import Data.CaseInsensitive (mk)
import qualified System.IO.Streams as Streams (toList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS


getHeader :: MonadSnap m => T.Text -> m (Maybe BS.ByteString)
getHeader headerName = Snap.getHeader (mk $ T.encodeUtf8 headerName) <$> Snap.getRequest



-- | Redirect to the specified frontend route
frontendRedirect
  :: forall beR cfg m frontendRoute.
     ( MonadSnap m
     , HasConfig cfg BaseURL
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     )
  => R frontendRoute
  -> ReaderT cfg m ()
frontendRedirect dest = do
  link_ <- renderFullRouteFE @beR dest
  liftSnap $ Snap.redirect $ T.encodeUtf8 . getLink $ link_

-- -- | Redirect to the specified frontend route
-- frontendRedirect :: MonadSnap m => T.Text -> R FrontendRoute -> m ()
-- frontendRedirect baseRoute dest = do
--   let href = T.encodeUtf8 $ (baseRoute <>) $ renderFrontendRoute checkedFullRouteEncoder dest
--   liftSnap $ Snap.redirect href

getRequestBodyJSON :: forall a m. (FromJSON a, MonadSnap m) => m (Either String a)
getRequestBodyJSON = Aeson.eitherDecode <$> getRequestBody


getRequestBody :: MonadSnap m => m LBS.ByteString
getRequestBody = LBS.fromChunks <$> runRequestBody Streams.toList

writeJSON' :: forall m e. (ToJSON e, MonadSnap m) => Either (BackendError e) () -> m ()
writeJSON' = writeJSON

withAuthdCORS :: Snap () -> Snap ()
withAuthdCORS handler = do
  liftIO $ putStrLn "withAuthdCORS"
  accessControlAllowThisOrigin
  modifyResponse $ setHeader "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
  --modifyResponse $ setHeader "Access-Control-Allow-Headers" "Content-Type, Authorization, Set-Cookie, X-Custom-Header"
  modifyResponse $ setHeader "Access-Control-Allow-Headers" "Content-Type, Authorization, Set-Cookie, X-Custom-Header, InterviewID, Question, Question-Number, X-ClientType"


  modifyResponse $ setHeader "Access-Control-Allow-Credentials" "true"
  Snap.method Snap.OPTIONS (writeBS "OK") <|> handler
  -- (liftIO $ putStr "DUMPED") >> dumpResponse

-- | Ensure that client is the same from request to request
accessControlAllowThisOrigin :: Snap ()
accessControlAllowThisOrigin = do
  originHeader <- Snap.getHeader "Origin" <$> getRequest
  liftIO $ do
    putStr "Header: "
    print originHeader
  case originHeader of
    Just origin -> do
      liftIO $ putStrLn "modify Access-Control-Allow-Origin"
      modifyResponse $ setHeader "Access-Control-Allow-Origin" $ origin
    Nothing     -> do
      liftIO $ putStrLn "No Origin header found"
      pure () -- writeBS "No Origin header found"
