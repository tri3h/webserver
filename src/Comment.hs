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

get :: QueryText -> IO Response
get query = do 
    case getInteger query "post_id" of 
        Right postId -> do 
            result <- Handler.get handle postId
            case result of 
                Right r -> return $ responseLBS status200 [(hContentType, "application/json")] $ encode r
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

create :: QueryText -> IO Response
create query = do 
    let isComment = getInteger query "post_id" >>= 
            \postId -> getInteger query "user_id" >>=
            \userId -> getText query "text" >>=
            \text -> Right $ CreateComment {
                postId = postId,
                userId = userId,
                text = text
                }
    case isComment of
        Right comment -> do 
            result <- Handler.create handle comment 
            case result of 
                Right r -> return $ responseLBS status200 [] ""
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: QueryText -> IO Response
delete query = do 
    case getInteger query "comment_id" of 
        Right commId -> do 
            result <- Handler.delete handle commId
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
