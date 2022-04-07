{-# LANGUAGE OverloadedStrings #-}
module Image where

import Types.Image ( Image(Image) )
import Network.HTTP.Types.URI ( QueryText )
import qualified Database.Queries.Image as Db
import Data.Text ( append, Text, pack )
import Data.Text.Lazy (fromStrict)
import Utility ( getInteger )
import Database.Connection ( manage )
import Network.Wai ( Response, responseBuilder )
import Network.HTTP.Types.Status ( status200, status400 )
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Network.HTTP.Types (queryToQueryText, hContentType)
import Data.Binary.Builder (fromByteString)
import Data.ByteString.Base64 (decode)
import qualified Handlers.Logger as Logger

get :: Logger.Handle IO -> QueryText -> IO Response
get logger query = do
    let info = getInteger query "image_id"
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of
        Right imageId -> do
            (Image image imageType) <- manage $ Db.get imageId
            let decodeImage = decode $ encodeUtf8 image
            Logger.debug logger $ "Tried to cget image and got: " ++ show decodeImage
            case decodeImage of
                Left l -> return $ responseBuilder status400 
                    [] . fromByteString $ encodeUtf8 $ pack l
                Right r -> return $ responseBuilder status200 
                    [(hContentType, encodeUtf8 $ "image/" `append` imageType)] $ fromByteString r
        Left l -> return $ responseBuilder status400 [] . fromByteString $ encodeUtf8 l
