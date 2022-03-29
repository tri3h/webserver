{-# LANGUAGE OverloadedStrings #-}
module Tag where

import Utility
import Types.Tag
import Database.Queries.Tag
import qualified Handler.Tag as Handler
import qualified Database.Queries.Tag as Db
import Database.Connection
import Data.Aeson
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text ( Text, unpack )

create :: QueryText -> IO Response
create query = do
    case getText query "name" of 
        Right name ->  do
            result <- Handler.createTag handle name
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> return $ responseLBS status200 [] ""
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

get :: QueryText -> IO Response
get query = do
    case getInteger query "tag_id" of 
        Right tagId -> do 
            res' <- Handler.getTag handle tagId
            case res' of 
                Right r' -> return $ responseLBS status200 [(hContentType, "application/json")] $ encode r'
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

edit :: QueryText -> IO Response
edit query = do
    let isTag = getText query "name" >>= 
            \name -> getInteger query "tag_id" >>=
            \tagId -> Right (tagId,name)
    case isTag of 
        Right (tagId, name) -> do 
            let tag = Tag {
                tagId = tagId,
                name = name
            }
            res' <- Handler.editTag handle tag
            case res' of 
                Right r' -> return $ responseLBS status200 [] ""
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: QueryText -> IO Response
delete query = do
    case getInteger query "tagId"  of 
        Right tagId -> do 
            res' <- Handler.deleteTag handle tagId
            case res' of
                Right r' -> return $ responseLBS status200 [] ""
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

handle :: Handler.Handle IO
handle = Handler.Handle {
    Handler.create = manage . Db.create,
    Handler.get = manage . Db.get,
    Handler.delete = manage . Db.delete,
    Handler.edit = manage . Db.edit,
    Handler.doesExist = manage . Db.doesExist
}
