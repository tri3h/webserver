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

create :: Query -> IO Response
create query = do
    let res = queryToList query ["name"]
    case res of
        Right r ->  do
            let tag = CreateTag {
                name = head r
            }
            result <- Handler.createTag handle tag
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> return $ responseLBS status200 [] ""
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

get :: Query -> IO Response
get query = do 
    let res = queryToList query ["tag_id"]
    case res of 
        Right r -> do 
            res' <- Handler.getTag handle (read . unpack $ head r :: Integer)
            case res' of 
                Right r' -> return $ responseLBS status200 [(hContentType, "application/json")] $ encode r'
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

edit :: Query -> IO Response
edit query = do 
    let res = queryToList query ["tag_id", "name"]
    case res of 
        Right r -> do 
            let tag = EditTag {
                tagId = read . unpack $ head r :: Integer,
                name = r !! 1
            }
            res' <- Handler.editTag handle tag
            case res' of 
                Right r' -> return $ responseLBS status200 [] ""
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: Query -> IO Response
delete query = do 
    let res = queryToList query ["tag_id"]
    case res of 
        Right r -> do 
            res' <- Handler.deleteTag handle (read . unpack $ head r :: Integer)
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
