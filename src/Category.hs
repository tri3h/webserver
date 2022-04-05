{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Category where

import Utility ( getText, getInteger, getMaybeInteger )
import Types.Category
    ( Category(Category, CategoryToCreate, categoryId, name,
               parentId) )
import qualified Handler.Category as Handler
import qualified Database.Queries.Category as Db
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
            let category = CategoryToCreate {
                parentId = getMaybeInteger query "parent_id",
                .. }
            result <- Handler.create handle category
            case result of
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> return $ responseLBS status201 [] ""
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

get :: QueryText -> IO Response
get query = do
    case getInteger query "category_id" of
        Right categoryId -> do
            result <- Handler.get handle categoryId
            case result of
                Right r -> return $ responseLBS status200 
                    [(hContentType, "application/json")] $ encode r
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

edit :: QueryText -> IO Response
edit query = do
    let info = do 
            categoryId <- getInteger query "category_id"
            name <- getText query "name"
            Right (categoryId, name)
    case info of
        Right (categoryId, name) -> do
            let category = Category {
                parentId = getMaybeInteger query "parent_id",
                ..
            }
            result <- Handler.edit handle category
            case result of
                Right _ -> return $ responseLBS status201 [] ""
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: QueryText -> IO Response
delete query = do
    case getInteger query "category_id" of
        Right categoryId -> do
            result <- Handler.delete handle categoryId
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
    Handler.hDoesExist = manage . Db.doesExist,
    Handler.hGetChildren = manage . Db.getChildren
}
