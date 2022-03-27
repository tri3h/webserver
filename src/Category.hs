{-# LANGUAGE OverloadedStrings #-}
module Category where

import Utility
import Types.Category
import qualified Handler.Category as Handler
import qualified Database.Queries.Category as Db
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
            let categ = CreateCategory {
                name = name,
                parentId = getMaybeInteger query "parent_id"
            }
            result <- Handler.createCategory handle categ
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> return $ responseLBS status200 [] ""
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

get :: QueryText -> IO Response
get query = do
    case getInteger query "category_id" of
        Right categId -> do
            res' <- Handler.getCategory handle categId
            case res' of
                Right r' -> return $ responseLBS status200 [(hContentType, "application/json")] $ encode r'
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

edit :: QueryText -> IO Response
edit query = do
    let isData = getInteger query "category_id" >>=
            \categId -> getText query "name" >>=
            \name -> Right (categId, name)
    case isData of
        Right (categId, name) -> do
            let categ = GetCategory {
                categoryId = categId,
                name = name,
                parentId = getMaybeInteger query "parent_id"
            }
            res' <- Handler.editCategory handle categ
            case res' of
                Right r' -> return $ responseLBS status200 [] ""
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: QueryText -> IO Response
delete query = do
    case getInteger query "category_id" of
        Right categId -> do
            res' <- Handler.deleteCategory handle categId
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
    Handler.doesExist = manage . Db.doesExist,
    Handler.getChildren = manage . Db.getChildren
}
