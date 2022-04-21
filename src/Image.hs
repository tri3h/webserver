{-# LANGUAGE OverloadedStrings #-}

module Image where

import Data.Binary.Builder (fromByteString)
import Data.ByteString.Base64 (decode)
import Data.Text (append, pack)
import Data.Text.Encoding (encodeUtf8)
import Database.Connection (manage)
import qualified Database.Queries.Image as Db
import qualified Handlers.Logger as Logger
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (status200, status400)
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response, responseBuilder)
import Types.Config (Config (database))
import Types.Image (Image (Image))
import Utility (getInteger)

get :: Logger.Handle IO -> Config -> QueryText -> IO Response
get logger config query = do
  let info = getInteger query "image_id"
  let db = database config
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right imageId -> do
      doesExist <- manage db $ Db.doesExist imageId
      case doesExist of
        Left l -> return $ responseBuilder status400 [] . fromByteString $ encodeUtf8 l
        Right _ -> do
          (Image image imageType) <- manage db $ Db.get imageId
          let decodeImage = decode $ encodeUtf8 image
          send decodeImage imageType
    Left l -> return $ responseBuilder status400 [] . fromByteString $ encodeUtf8 l
  where
    send decodeImage imageType = case decodeImage of
      Left l ->
        return $
          responseBuilder
            status400
            []
            . fromByteString
            $ encodeUtf8 $ pack l
      Right r ->
        return $
          responseBuilder
            status200
            [(hContentType, encodeUtf8 $ "image/" `append` imageType)]
            $ fromByteString r
