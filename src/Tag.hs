{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Tag where

import Utility ( getText, getInteger )
import Types.Tag ( Tag(Tag, tagId, name) )
import qualified Handler.Tag as Handler
import qualified Database.Queries.Tag as Db
import Database.Connection ( manage )
import Data.Aeson ( encode )
import Network.Wai ( Response, responseLBS )
import Network.HTTP.Types.Status
    ( status200, status201, status204, status400 )
import Network.HTTP.Types.Header ( hContentType )
import Network.HTTP.Types.URI ( QueryText )
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text ( Text, unpack )

create :: QueryText -> IO Response
create query = do
    case getText query "name" of 
        Right name ->  do
            result <- Handler.create handle name
            case result of
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
                Right _ -> return $ responseLBS status201 [] ""
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

get :: QueryText -> IO Response
get query = do
    case getInteger query "tag_id" of 
        Right tagId -> do 
            result <- Handler.get handle tagId
            case result of 
                Right r -> return $ responseLBS status200 
                    [(hContentType, "application/json")] $ encode r
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

edit :: QueryText -> IO Response
edit query = do
    let info = do 
            name <- getText query "name"
            tagId <- getInteger query "tag_id"
            Right (tagId,name)
    case info of 
        Right (tagId, name) -> do 
            let tag = Tag { .. }
            result <- Handler.edit handle tag
            case result of 
                Right _ -> return $ responseLBS status201 [] ""
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: QueryText -> IO Response
delete query = do
    case getInteger query "tag_id"  of 
        Right tagId -> do 
            result <- Handler.delete handle tagId
            case result of
                Right _ -> return $ responseLBS status204 [] ""
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

handle :: Handler.Handle IO
handle = Handler.Handle {
    Handler.hCreate = manage . Db.create,
    Handler.hGet = manage . Db.get,
    Handler.hDelete = manage . Db.delete,
    Handler.hEdit = manage . Db.edit,
    Handler.hDoesExist = manage . Db.doesExist
}
