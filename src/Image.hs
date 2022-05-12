{-# LANGUAGE OverloadedStrings #-}

module Image where

import Data.Binary.Builder (fromByteString)
import Data.Pool (Pool, withResource)
import Data.Text (Text, append)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Image as Db
import qualified Handlers.Logger as Logger
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (status200, status400)
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response, responseBuilder)
import Types.Image (Image (Image), ImageType)
import Utility (getInteger, getText)

get :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
get logger pool query = do
  let info = getInteger query "image_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right imageId -> do
      doesExist <- withResource pool $ Db.doesExist imageId
      case doesExist of
        Left l -> return $ responseBuilder status400 [] . fromByteString $ encodeUtf8 l
        Right _ -> do
          (Image image imageType) <- withResource pool $ Db.get imageId
          return $
            responseBuilder
              status200
              [(hContentType, encodeUtf8 $ "image/" `append` imageType)]
              $ fromByteString image
    Left l -> return $ responseBuilder status400 [] . fromByteString $ encodeUtf8 l

getImageType :: QueryText -> Either Text ImageType
getImageType query = getText query "image_type"
