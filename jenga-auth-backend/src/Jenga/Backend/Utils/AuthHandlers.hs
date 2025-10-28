module Jenga.Backend.Utils.AuthHandlers where


import Jenga.Backend.Utils.Query
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.DB.Auth
import Jenga.Backend.Utils.Snap
import Jenga.Common.Cookie
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.BeamExtras
import Jenga.Common.Schema

import Rhyolite.Account
import qualified Data.Signed.ClientSession as Sesh
import Web.ClientSession as CS

import Snap
import Snap.Extras (writeJSON)
import Database.Beam.Schema
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.Types (SqlSerial(..))
import Data.Signed
import Data.Signed.ClientSession as CSK

import Data.Pool
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.Either
import Data.Functor.Identity
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS


-- | Backend only (needs key)
authTokenToCookieValue
  :: CS.Key
  -> Id Account
  -> IO BS.ByteString
authTokenToCookieValue csk tkn = do
  signedAcctID <- Sesh.signWithKey csk tkn
  pure $ keylessTo signedAcctID

-- | Backend only (needs key)
authTokenFromCookieValue
  :: CS.Key
  -> BS.ByteString
  -> Maybe (Id Account)
authTokenFromCookieValue csk bs =
  (Sesh.readSignedWithKey csk)
  =<< keylessFrom bs


-- | This is currently a toss up as we switch from an old semi-broken system
-- | to a new one ... which may be also just as unknown
-- | this works but ... why does this happen??
removeQuotes :: T.Text -> T.Text
removeQuotes = T.dropWhile (== '"') . T.dropWhileEnd (== '"')
  -- or is it id
  -- or is it: T.init . (T.drop 1)

removeQuotesBS :: BS.ByteString -> BS.ByteString
removeQuotesBS =
  --BS.dropWhile (== '"') . BS.dropWhileEnd (== '"')
  T.encodeUtf8
  . removeQuotes
  . T.decodeUtf8


--- Cookie Authentication Handlers

privateRoute
  :: forall cfg m a.
     ( A.ToJSON a
     , MonadSnap m
     , HasConfig cfg AuthCookieName
     , HasConfig cfg CS.Key
     )
  => (Id Account -> ReaderT cfg m a)
  -> ReaderT cfg m ()
privateRoute fma = do
  let writeJSON'' :: MonadSnap m => Either (BackendError ()) () -> ReaderT cfg m ()
      writeJSON'' = lift . writeJSON

  authCookieName <- getAuthCookieName <$> asksM
  mCookie <- liftSnap $ getCookie $ T.encodeUtf8 authCookieName

  case mCookie of
    Nothing -> writeJSON'' $ (Left NoAuth_NoCookie)
    Just cookie -> do
      liftIO $ do
        print cookie
        print $ cookieValue cookie
        print $ B64.decode $ cookieValue cookie
      let
        --removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen ?
        signed :: Signed (PrimaryKey Rhyolite.Account.Account Identity)
        signed = Signed . removeQuotes . T.decodeUtf8 . (fromRight undefined) . B64.decode . cookieValue $ cookie
      key <- asksM --  _clientSessionKey
      case readSignedWithKey key signed of
        Nothing -> writeJSON'' $ (Left NoAuth_CantReadKey)
        Just acctId -> do
          _ <- fma acctId -- AcctID $ fromIntegral int64
          pure ()

-- setAuthCookieHeader :: MonadSnap m => Signed AccountId -> EnvT m ()
-- setAuthCookieHeader signedTokenUserID = do
--   --host <- decideHost
--   baseRoute <- asksCfg _baseRoute
--   let cookieValue_ = B64.encode . T.encodeUtf8 . unSigned $ signedTokenUserID
--   -- ace in case we change our domain... this should more likely than not still work
--   case T.isInfixOf "ace" (T.pack . show $ baseRoute) of
--     True -> do
--       let h = T.encodeUtf8
--               . T.strip

--               . T.pack
--               . fromMaybe (error "will never fire")
--               . uriHost $ baseRoute
--       Snap.modifyResponse $ Snap.setHeader "Set-Cookie"
--         $ T.encodeUtf8 authCookieName
--         <> "=" <> cookieValue_ <> "; "
--         <> "Path=/; "
--         -- We need to set this no matter what if acetalent.io to
--         -- ensure android and ios work properly
--         <> "Domain=" <> h <> "; "
--         <> "HttpOnly; Secure; SameSite=None"
--     False -> do
--       -- Either pure localhost or localhost via ngrok tunnel
--       ngrok <- asksCfg _ngrokRoute
--       case ngrok >>= uriHost of
--         Nothing -> do
--           Snap.modifyResponse $ Snap.setHeader "Set-Cookie"
--             $ T.encodeUtf8 authCookieName
--             <> "=" <> cookieValue_ <> "; "
--             <> "Path=/; "
--             -- Do we need the subdomain?
--             -- <> "Domain=" <> "b674-207-136-101-252.ngrok-free.app" <> "; "
--             <> "HttpOnly; Secure; SameSite=None"
--         Just ngrokHost -> do
--           Snap.modifyResponse $ Snap.setHeader "Set-Cookie"
--             $ T.encodeUtf8 authCookieName
--             <> "=" <> cookieValue_ <> "; "
--             <> "Path=/; "
--             -- Do we need the subdomain?
--             <> "Domain=" <> (T.encodeUtf8 . T.pack $ ngrokHost) <> "; "
--             <> "HttpOnly; Secure; SameSite=None"


  -- originHeader <- Snap.getHeader "Origin" <$> getRequest
  -- liftIO $ print originHeader



-- | Is copy of privateRoute with allowed users
dependentPrivateRoute
  :: forall db m a.
     ( MonadSnap m
     , A.ToJSON a
     , Database Postgres db
     )
  => PgTable Postgres db UserTypeTable
  -> AuthCookieName
  -> Pool Connection
  -> CSK.Key
  -> (UserType -> Id Account -> m a)
  -> m ()
dependentPrivateRoute uTypeTbl (AuthCookieName authCookieName) dbConn key fma = do
  mCookieAID <- getCookie $ T.encodeUtf8 authCookieName
  case mCookieAID of
    Nothing -> do
      liftIO $ putStrLn "depeendentPrivateRoute: No Cookie Found"
      cookies_ <- rqCookies <$> getRequest
      liftIO $ print cookies_
      liftSnap $ writeJSON' $ (Left NoAuth_NoCookie :: Either (BackendError ()) ()) --  :: Either T.Text ())
    Just cookieAID -> do
      liftIO $ do
        print cookieAID
        print $ cookieValue cookieAID
        print $ B64.decode $ cookieValue cookieAID
        print $ B64.encode . fromRight undefined . B64.decode $ cookieValue cookieAID
      let
        --removeQuotes = id -- T.init . (T.drop 1) -- TODO: why does this happen ?
        signed :: Signed (PrimaryKey Account Identity)
        signed = Signed . removeQuotes . T.decodeUtf8 . (fromRight undefined) . B64.decode . cookieValue $ cookieAID
      case readSignedWithKey key signed of
        Nothing -> liftSnap $ writeJSON' $ (Left NoAuth_CantReadKey  :: Either (BackendError ()) ()) -- "couldnt read key" :: Either T.Text ())
        -- We still have this UserType on the frontend so that
        -- we can run conditional logic on what to show the user
        -- but for safety we ask the type ourselves as the user could just enter in a fake one
        Just acctId@(AccountId (SqlSerial _)) -> do
          userType' <- runSerializable dbConn $ getUserType uTypeTbl acctId
          case userType' of
            Nothing -> liftSnap $ writeJSON' $ (Left NoAuth_NoUserTypeCookie :: Either (BackendError ()) ())
            Just userType -> do
              _ <- fma userType acctId
              pure ()

-- NOTE: could easily generalize this if we ever have other signed data in headers?
getAccountIdFromCookies
  :: ( A.ToJSON e
     , MonadSnap m
     , HasConfig cfg CS.Key
     , HasConfig cfg AuthCookieName
     )
  => ReaderT cfg m (Either (BackendError e) AccountId)
getAccountIdFromCookies = do
  cskey <- asksM -- asksCfg _clientSessionKey
  authCookieName <- getAuthCookieName <$> asksM
  mCookie <- getCookie $ T.encodeUtf8 authCookieName
  case mCookie of
    Nothing -> pure $ Left NoAuth_NoCookie
    Just cookieSignedId -> do
      let
        --removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen ?
        eithDecodedB64 = B64.decode . cookieValue $ cookieSignedId
      case eithDecodedB64 of
        Left _decodeErr -> pure $ Left NoAuth_CantReadKey -- "Base64 Decode Error: " <> T.pack decodeErr
        Right decodedB64 -> do
          let
            -- for type annotation
            signed :: Signed (PrimaryKey Account Identity)
            signed = Signed . removeQuotes . T.decodeUtf8 $ decodedB64
          case readSignedWithKey cskey signed of
            Nothing -> pure $ Left NoAuth_CantReadKey
            Just acctId -> pure $ Right acctId

-- | is copy of privateRoute with allowed users
constrainedPrivateRoute
  :: forall db cfg m a.
     ( MonadSnap m
     , Database Postgres db
     , HasConfig cfg CS.Key
     , HasConfig cfg AuthCookieName
     , HasJengaTable Postgres db UserTypeTable
     )
  => Pool Connection
  -> Key
  -> NonEmpty.NonEmpty UserType
  -> (Id Account -> ReaderT cfg m a)
  -> ReaderT cfg m ()
constrainedPrivateRoute dbConn key userTypesAllowed fma = do
  let writeJSON'' :: forall x e. (A.ToJSON x, A.ToJSON e) => Either (BackendError e) x -> Snap ()
      writeJSON'' = writeJSON
  authCookieName <- getAuthCookieName <$> asksM
  mCookieAID <- getCookie $ T.encodeUtf8 authCookieName
  case mCookieAID of
    Nothing -> liftSnap $ do
      req <- getRequest

      liftIO $ do
        putStr "constrainedPrivateRoute: getCookie failed"
        print req
        putStrLn "------------------"
        print $ rqCookies req
      writeJSON'' $ (Left NoAuth_NoCookie :: Either (BackendError ()) ())
    Just cookieAID -> do
      let
        --removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen / why do we need this drop?
        signed :: Signed (PrimaryKey Account Identity)
        signed = Signed . removeQuotes . T.decodeUtf8 . (fromRight undefined) . B64.decode . cookieValue $ cookieAID
      case readSignedWithKey key signed of
        Nothing -> liftSnap $ writeJSON'' $ (Left NoAuth_CantReadKey :: Either (BackendError ()) ())
        -- We still have this UserType on the frontend so that
        -- we can run conditional logic on what to show the user
        -- but for safety we ask the type ourselves as the user could just enter in a fake one
        Just acctId@(AccountId (SqlSerial _)) -> do
          (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
          userType' <- runSerializable dbConn $ getUserType uTypeTbl acctId
          case userType' of
            Nothing -> liftSnap $ writeJSON'' $ (Left NoAuth_NoUserTypeCookie :: Either (BackendError ()) ())
            Just userType -> do
              case elem userType userTypesAllowed of
                False -> pure ()
                True -> do
                  _ <- fma acctId
                  pure ()

privateRouteJSONOut
  :: forall cfg m a e.
     ( A.ToJSON a
     , A.ToJSON e
     , MonadSnap m
     , HasConfig cfg AuthCookieName
     )
  => Key
  -> (Id Account -> ReaderT cfg m (Either (BackendError e) a))
  -> ReaderT cfg m ()
privateRouteJSONOut key fma = do
  let writeJSON'' :: forall x1 e1. (A.ToJSON x1, A.ToJSON e1)
        => Either (BackendError e1) x1 -> Snap ()
      writeJSON'' = writeJSON
  authCookieName <- getAuthCookieName <$> asksM
  mCookie <- getCookie $ T.encodeUtf8 authCookieName
  liftIO $ putStrLn "LINE 237"
  liftIO $ print mCookie
  case mCookie of
    Nothing -> do
      cs <- liftSnap $ pure . rqCookies =<< getRequest
      liftIO $ putStrLn "Cookies:" >> print cs
      liftSnap $ writeJSON'' $ (Left NoAuth_NoCookie :: Either (BackendError e) a)
    Just cookieAID -> do
      liftIO $ putStrLn "NEW"
      liftIO $ print
        $ authTokenFromCookieValue key $ cookieValue cookieAID

      let
        --removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen ?
        signed :: Signed (PrimaryKey Account Identity)
        signed = Signed . T.decodeUtf8 . (fromRight undefined) . B64.decode . cookieValue $ cookieAID

      liftIO $ putStrLn "Debug Cookies:"
      liftIO $ print $ cookieValue cookieAID
      liftIO $ print $ B64.decode $ cookieValue cookieAID
      liftIO $ print $ fromRight undefined . B64.decode $ cookieValue cookieAID

      liftIO $ putStrLn "TRY Cookies"

      liftIO $ print
        $ (readSignedWithKey key :: Signed (PrimaryKey Account Identity)
                                 -> Maybe (PrimaryKey Account Identity)
          )
        . Signed . T.decodeUtf8 . fromRight undefined . B64.decode . removeQuotesBS
        $ cookieValue cookieAID
      case (readSignedWithKey key) signed of
        Nothing -> liftSnap $ writeJSON'' $ (Left NoAuth_CantReadKey :: Either (BackendError e) a)
        Just acctId@(AccountId (SqlSerial _)) -> do
          a <- fma acctId
          liftSnap $ writeJSON'' a
