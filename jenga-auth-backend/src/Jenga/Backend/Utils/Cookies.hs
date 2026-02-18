{-# LANGUAGE OverloadedStrings #-}

module Jenga.Backend.Utils.Cookies
  ( addAuthCookieHeader
  , addUserTypeCookieHeader
  , authTokenToCookieValue
  , authTokenFromCookieValue
  ) where

import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.Snap (getHeader)
import Jenga.Common.Auth
import Jenga.Common.BeamExtras
import Jenga.Common.Cookie (keylessTo, keylessFrom)

import Rhyolite.Account
import qualified Data.Signed.ClientSession as Sesh
import qualified Snap
import Web.ClientSession as CS
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Aeson as Aeson
--import Data.Maybe (fromMaybe)

-- | Set the authentication cookie
addAuthCookieHeader
  :: ( MonadIO m
     , Snap.MonadSnap m
     , HasConfig cfg Key
     , HasConfig cfg DomainOption
     )
  => T.Text
  -> Id Account
  -> ReaderT cfg m ()
addAuthCookieHeader cookieName_ acctID = do
  liftIO $ putStrLn $ "addAuthCookieHeader called for account: " <> show acctID
  domainOpts <- asksM -- Cfg _domainName
  csk <- asksM -- clientSessionKey
  cookieValue_ <- liftIO $ authTokenToCookieValue csk acctID
  domain <- askDomainKVSnap domainOpts

  let cookie = Snap.Cookie
        { Snap.cookieName = T.encodeUtf8 cookieName_
        , Snap.cookieValue = cookieValue_
        , Snap.cookieExpires = Nothing
        , Snap.cookieDomain = fmap (BS.drop 7 . BS.takeWhile (/= 59)) domain -- Extract domain from "Domain=localhost; "
        , Snap.cookiePath = Just "/"
        , Snap.cookieSecure = True
        , Snap.cookieHttpOnly = False
        }

  liftIO $ putStrLn $ "Setting auth cookie: " <> show cookie
  -- Use addResponseCookie which creates separate Set-Cookie headers
  Snap.modifyResponse $ Snap.addResponseCookie cookie
  liftIO $ putStrLn "Auth cookie added via addResponseCookie"
  pure () --signedKey
--     <> "HttpOnly; Secure; SameSite=None"

-- | Set the user type cookie
addUserTypeCookieHeader
  :: ( Snap.MonadSnap m
     , HasConfig cfg DomainOption
     )
  => T.Text
  -> UserType
  -> ReaderT cfg m ()
addUserTypeCookieHeader cookieName_ userType = do
  liftIO $ putStrLn $ "addUserTypeCookieHeader called for user type: " <> show userType
  domainOpts <- asksM
  domain <- askDomainKVSnap domainOpts
  let userTypeJsonBytes = LBS.toStrict $ Aeson.encode userType
      userTypeCookieValue = B64.encode userTypeJsonBytes

  let cookie = Snap.Cookie
        { Snap.cookieName = T.encodeUtf8 cookieName_
        , Snap.cookieValue = userTypeCookieValue
        , Snap.cookieExpires = Nothing
        , Snap.cookieDomain = fmap (BS.drop 7 . BS.takeWhile (/= 59)) domain -- Extract domain from "Domain=localhost; "
        , Snap.cookiePath = Just "/"
        , Snap.cookieSecure = True
        , Snap.cookieHttpOnly = False
        }

  liftIO $ putStrLn $ "Setting user type cookie: " <> show cookie
  -- Use addResponseCookie which creates separate Set-Cookie headers
  Snap.modifyResponse $ Snap.addResponseCookie cookie
  liftIO $ putStrLn "User type cookie added via addResponseCookie"
  pure ()

-- | Get domain settings for cookie from Snap request
askDomainKVSnap :: Snap.MonadSnap m => DomainOption -> m (Maybe BS.ByteString)
askDomainKVSnap d = do
  mClientString <- getHeader clientTypeHeader
  pure $ case mClientString of
    Nothing -> getDomainKV (Web, d)
    Just "Mobile" -> getDomainKV (Mobile, d)
    Just "Web" -> getDomainKV (Web, d)
    _ -> Nothing
  where
    getDomainKV :: (ClientType, DomainOption) -> Maybe BS.ByteString
    getDomainKV = \case
      (Mobile, ProxiedDomain _ _) -> Nothing
      x@(Web, ProxiedDomain _ _) -> Just $ "Domain=" <> (T.encodeUtf8 $ getDomainName x) <> "; "
      x@(Web, DirectDomain _) -> Just $ "Domain=" <> (T.encodeUtf8 $ getDomainName x) <> "; "
      x@(Mobile, DirectDomain _) -> Just $ "Domain=" <> (T.encodeUtf8 $ getDomainName x) <> "; "

-- | Get the domain name based on client type and domain options
getDomainName :: (ClientType, DomainOption) -> T.Text
getDomainName = \case
  (Mobile, ProxiedDomain proxyDomainName _baseDomainName) -> proxyDomainName
  (Mobile, DirectDomain domainName) -> domainName
  (Web, ProxiedDomain _proxyDomainName baseDomainName) -> baseDomainName
  (Web, DirectDomain domainName) -> domainName

-- | Convert an account ID to a signed cookie value
authTokenToCookieValue
  :: CS.Key
  -> Id Account
  -> IO BS.ByteString
authTokenToCookieValue csk tkn = do
  signedAcctID <- Sesh.signWithKey csk tkn
  pure $ keylessTo signedAcctID

-- | Convert a cookie value to an account ID
authTokenFromCookieValue
  :: CS.Key
  -> BS.ByteString
  -> Maybe (Id Account)
authTokenFromCookieValue csk bs =
  (Sesh.readSignedWithKey csk)
  =<< keylessFrom bs
