module Jenga.Common.Cookie where

--experimental
--import Data.Map as Map
--import Database.Beam.Postgres
--import Common.Types
import Jenga.Common.BeamExtras
import Data.String
import Data.Signed
import Rhyolite.Account --hiding (Beamable, PrimaryKey)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
--import Web.ClientSession as CS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as T

clientTypeHeader :: IsString s => s
clientTypeHeader = "X-ClientType"

-- | Assert phantom type
-- | Do we ever actually need this???
-- |   -> only if we SetCookie... but this
-- |      is now handled by backend
keylessTo :: Signed (Id Account) -> BS.ByteString
keylessTo = keylessTo'

-- | Assert phantom type
keylessFrom :: BS.ByteString -> Maybe (Signed (Id Account))
keylessFrom = keylessFrom'

keylessTo' :: Signed a -> BS.ByteString
keylessTo' signedAcctID =
  B64.encode .
  T.encodeUtf8 $
  unSigned signedAcctID

keylessFrom' :: BS.ByteString -> Maybe (Signed a)
keylessFrom' bs_ =
 fmap (Signed . T.decodeUtf8)
 $ eithToMaybe
 $ B64.decode bs_
  where
    eithToMaybe = \case { Right x -> Just x ; Left _ -> Nothing }

-- | Signed is a phantom type. We dont actually
-- | touch its inner text
signedToJSON :: Signed a -> LBS.ByteString
signedToJSON = A.encode

-- | Signed is a phantom type. We dont actually
-- | touch its inner text
signedFromJSON :: LBS.ByteString -> Maybe (Signed a)
signedFromJSON = A.decode
