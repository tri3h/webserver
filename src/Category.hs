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

create :: Query -> IO Response
create query = do
    let res = queryToList query ["name"]
    let resExtra = queryToMaybeList query ["parent_id"]
    case res of
        Right r ->  do
            let cat = CreateCategory {
                name = head r,
                parentId = (\a -> read a :: Integer) . unpack <$> head resExtra
            }
            result <- Handler.createCategory handle cat
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> return $ responseLBS status200 [] ""
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

get :: Query -> IO Response
get query = do
    let res = queryToList query ["category_id"]
    case res of
        Right r -> do
            res' <- Handler.getCategory handle (read . unpack $ head r :: Integer)
            case res' of
                Right r' -> return $ responseLBS status200 [(hContentType, "application/json")] $ encode r'
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

edit :: Query -> IO Response
edit query = do
    let res = queryToList query ["category_id", "name"]
    let resExtra = queryToMaybeList query ["parent_id"]
    case res of
        Right r -> do
            let cat = GetCategory {
                categoryId = read . unpack $ head r :: Integer,
                name = r !! 1,
                parentId = (\a -> read a :: Integer) . unpack <$> head resExtra
            }
            res' <- Handler.editCategory handle cat
            case res' of
                Right r' -> return $ responseLBS status200 [] ""
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: Query -> IO Response
delete query = do
    let res = queryToList query ["category_id"]
    case res of
        Right r -> do
            res' <- Handler.deleteCategory handle (read . unpack $ head r :: Integer)
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
