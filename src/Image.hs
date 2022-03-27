{-# LANGUAGE OverloadedStrings #-}
module Image where

import Types.Image
import Network.HTTP.Types.URI
import Network.Wai
import qualified Database.Queries.Image as Db
import Data.Text(append)
import Data.Text.Lazy (fromStrict)
import Utility
import Database.Connection
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types.Status
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Data.Text ( Text )
import Control.Monad
import qualified Data.ByteString as BS
import Network.HTTP.Types (queryToQueryText, hContentType)
import Data.Binary.Builder (fromByteString)

get :: QueryText -> IO Response
get query = do
    case getInteger query "image_id" of
            Right imageId -> do
                (Image image) <- manage $ Db.get imageId
                let html = "<img src=\"data:image/png;base64," `append` image `append` "\"/>"
                return $ responseBuilder status200 [(hContentType, "text/html")] . fromByteString $ encodeUtf8 html
            Left l -> return $ responseBuilder status400 [] . fromByteString $ encodeUtf8 l
