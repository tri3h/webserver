{-# LANGUAGE OverloadedStrings #-}
module Comment where

import qualified Handler.Comment as Handler
import Database.Connection
import qualified Database.Queries.Comment as Db
import Types.Comment
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Utility
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text.Encoding ( decodeUtf8, decodeUtf16BE )
import qualified Data.Text.Lazy as LazyText
import Data.Text (unpack)
import Data.Aeson

get :: Query -> IO Response
get query = do 
    let res = queryToList query ["post_id"]
    case res of 
        Right r -> do 
            result <- Handler.get handle (read (unpack $ head r) :: Integer)
            case result of 
                Right r -> return $ responseLBS status200 [(hContentType, "application/json")] $ encode r
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

create :: Query -> IO Response
create query = do 
    let res = queryToList query ["post_id", "user_id", "text"]
    case res of 
        Right r -> do 
            let comment = CreateComment {
                postId = read (unpack $ head r) :: Integer,
                userId = read (unpack $ r !! 1) :: Integer,
                text = r !! 2
            }
            result <- Handler.create handle comment 
            case result of 
                Right r -> return $ responseLBS status200 [] ""
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: Query -> IO Response
delete query = do 
    let res = queryToList query ["comment_id"]
    case res of 
        Right r -> do 
            result <- Handler.delete handle (read (unpack $ head r) :: Integer)
            case result of 
                Right r -> return $ responseLBS status200 [] ""
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

handle :: Handler.Handle IO
handle = Handler.Handle {
    Handler.hGet = manage . Db.get,
    Handler.hCreate = manage . Db.create,
    Handler.hDelete = manage . Db.delete
}
