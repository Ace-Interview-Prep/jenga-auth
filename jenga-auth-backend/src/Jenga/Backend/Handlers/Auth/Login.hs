{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Jenga.Backend.Handlers.Auth.Login where


-- import Backend.DB (db)
import Jenga.Backend.DB.Auth
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
-- import Common.Constants (clientTypeHeader)
import Jenga.Common.Schema
-- import Common.Constants (authCookieName)
-- import Common.Types
-- import Common.ChatSchema
import Jenga.Backend.Utils.AuthHandlers
import Jenga.Common.BeamExtras
import Jenga.Common.Auth
import Jenga.Common.Errors
import Jenga.Backend.Utils.Snap (getHeader)

import Rhyolite.Backend.Account
import Rhyolite.Account

import qualified Snap

import Database.Beam.Postgres
import Database.Beam.Schema
import Data.Signed.ClientSession

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Pool
import Data.Signed
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import Text.Read (readMaybe)
import Data.Maybe


-- | Note that the login function from rhyolite really just returns the ID
loginHandler
  :: forall db cfg m.
     ( MonadIO m
     , Database Postgres db
     , Snap.MonadSnap m
     , HasConfig cfg DomainOption
     , HasConfig cfg AuthCookieName
     , HasConfig cfg Key
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db Account
     )
  => (T.Text, Password)
  -> ReaderT cfg m (Either (BackendError LoginError) (Signed (Id Account), UserType))
loginHandler (email, Password pass) = do
  tryLogin @db (email, Password pass) >>= \case
    Left bError -> pure $ Left bError
    Right (usersAccountId, userType) -> do
      csk <- asksM
      authCookieName <- getAuthCookieName <$> asksM
      signedTokenUserID_  <- liftIO $ signWithKey csk usersAccountId
      addAuthCookieHeader authCookieName usersAccountId
      pure $ Right (signedTokenUserID_, userType)

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
  domainOpts <- asksM -- Cfg _domainName
  csk <- asksM -- clientSessionKey
  cookieValue_ <- liftIO $ authTokenToCookieValue csk acctID
  domain <- askDomainKVSnap domainOpts
  Snap.modifyResponse $ Snap.setHeader "Set-Cookie"
    $ (T.encodeUtf8 $ cookieName_)
    <> "=" <> cookieValue_ <> "; "
    <> "Path=/; "
    <> fromMaybe "" domain
    -- <> "Domain=" <> (T.encodeUtf8 domain) <> "; "
    <> "Secure; "
    <> "SameSite=None"
  pure () --signedKey
--     <> "HttpOnly; Secure; SameSite=None"

askDomainNameSnap :: Snap.MonadSnap m => DomainOption -> m T.Text
askDomainNameSnap d = do
  mClientString <- (getHeader clientTypeHeader)
  pure $ case mClientString of
    Nothing -> getDomainName (Web, d)
    Just clientTypeString -> case readMaybe . T.unpack . T.decodeUtf8 $ clientTypeString of
      Nothing -> getDomainName (Web, d)
      Just clientType -> getDomainName (clientType, d)

askDomainKVSnap
  :: Snap.MonadSnap m
  => DomainOption
  -> m (Maybe BS.ByteString)
askDomainKVSnap d = do
  mClientString <- (getHeader clientTypeHeader)
  let clientType = fromMaybe Web $ readMaybe . T.unpack . T.decodeUtf8 =<< mClientString
  pure $ case (clientType, d) of
    (Mobile, ProxiedDomain _ _) -> Nothing
    x@(Web, ProxiedDomain _ _) ->
      -- This is untested
      Just $ "Domain=" <> (T.encodeUtf8 $ getDomainName x) <> "; "
    x@(Web, DirectDomain _) ->
      Just $ "Domain=" <> (T.encodeUtf8 $ getDomainName x) <> "; "
    x@(Mobile, DirectDomain _) ->
      Just $ "Domain=" <> (T.encodeUtf8 $ getDomainName x) <> "; "


-- | If we are using websockets, this is all we need
tryLogin
  :: forall db cfg m.
     ( MonadIO m
     , Database Postgres db
     , HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db Account
     )
  => (T.Text, Password)
  -> ReaderT cfg m (Either
                    (BackendError LoginError)
                    (Id Account, UserType)
                   )
tryLogin (email, Password pass) = do
  (acctsTbl :: PgTable Postgres db Account) <- asksTableM
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  mAcct <- withDbEnv $ getUserByEmail acctsTbl email
  case mAcct of
    Nothing -> pure . Left . BUserError $ UnrecognizedEmail email
    Just acctRow@(Rhyolite.Account.Account _ userEmail _ _) -> do
      loginResult <- withDbEnv $ login acctsTbl (T.toLower userEmail) pass
      case loginResult of
        Nothing -> pure . Left . BUserError $ IncorrectPassword
        Just usersAccountId -> do
          userType' <- withDbEnv $ getUserType uTypeTbl $ pk acctRow--aid
          case userType' of
            Nothing -> pure . Left . BCritical $ NoUserTypeFound
            Just Admin -> do
              pure $ Right (usersAccountId, Admin)
            Just Self -> do
              pure $ Right (usersAccountId, Self)

-- | TODO: is this a prism?
getDomainName :: (ClientType, DomainOption) -> T.Text
getDomainName = \case
  (Mobile, ProxiedDomain proxyDomainName _baseDomainName) -> proxyDomainName
  (Web   , ProxiedDomain _proxyDomainName baseDomainName) -> baseDomainName
  (_     , DirectDomain u) -> u
