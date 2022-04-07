{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Comment where

import qualified Handlers.Comment as Handler
import Database.Connection ( manage )
import qualified Database.Queries.Comment as Db
import qualified Database.Queries.Post as Db.Post
import Types.Comment
    ( Comment(CommentToCreate, postId, userId, text) )
import Network.Wai ( Response, responseLBS )
import Network.HTTP.Types.Status
    ( status200, status201, status204, status400 )
import Network.HTTP.Types.Header ( hContentType )
import Network.HTTP.Types.URI ( QueryText )
import Utility ( getText, getInteger )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text.Encoding ( decodeUtf8, decodeUtf16BE )
import qualified Data.Text.Lazy as LazyText
import Data.Text (unpack)
import Data.Aeson ( encode )
import qualified Handlers.Logger as Logger

create :: Logger.Handle IO -> QueryText -> IO Response
create logger query = do 
    let info = do 
            postId <- getInteger query "post_id"
            userId <- getInteger query "user_id" 
            text <- getText query "text" 
            Right CommentToCreate { .. }
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of
        Right comment -> do 
            result <- Handler.create handle comment 
            Logger.debug logger $ "Tried to create comment and got: " ++ show result
            case result of 
                Right _ -> return $ responseLBS status201 [] ""
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

get :: Logger.Handle IO -> QueryText -> IO Response
get logger query = do 
    let info = getInteger query "post_id"
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of 
        Right postId -> do 
            result <- Handler.get handle postId
            Logger.debug logger $ "Tried to get comment and got: " ++ show result
            case result of 
                Right r -> return $ responseLBS status200 
                    [(hContentType, "application/json")] $ encode r
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: Logger.Handle IO -> QueryText -> IO Response
delete logger query = do 
    let info = getInteger query "comment_id"
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of 
        Right commentId -> do 
            result <- Handler.delete handle commentId
            Logger.debug logger $ "Tried to delete comment and got: " ++ show result
            case result of 
                Right _ -> return $ responseLBS status204 [] ""
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

handle :: Handler.Handle IO
handle = Handler.Handle {
    Handler.hGet = manage . Db.get,
    Handler.hCreate = manage . Db.create,
    Handler.hDelete = manage . Db.delete,
    Handler.hDoesPostExist = manage . Db.Post.doesExist,
    Handler.hDoesExist = manage . Db.doesExist
}
