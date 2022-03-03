{-# LANGUAGE OverloadedStrings #-}
module Author where

import qualified Handler.Author as Handler
import Types.Author
import qualified Database.Queries.Author as Db
import qualified Database.Queries.User as UserDb
import Database.Connection
import Data.Aeson
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Text.Lazy as LazyText
import Data.Text ( Text, unpack )
import Control.Monad
import Data.ByteString.Lazy ( append )
import Utility


create :: Query -> IO Response
create query = do
    let res = queryToList query ["user_id", "description"]
    case res of 
        Right r ->  do
            let author = CreateAuthor {
                userId = read . unpack $ head r :: Integer,
                description = r !! 1
            }
            result <- Handler.createAuthor handle author
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> return $ responseLBS status200 [] ""
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

get :: Query -> IO Response
get query = do 
    let res = queryToList query ["author_id"]
    case res of 
        Right r -> do 
            res' <- Handler.getAuthor handle (read . unpack $ head r :: Integer)
            case res' of 
                Right r' -> return $ responseLBS status200 [(hContentType, "application/json")] $ encode r'
                --defaultOptions {sumEncoding = Untagged Value}
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

edit :: Query -> IO Response
edit query = do 
    let res = queryToList query ["author_id", "description"]
    case res of 
        Right r -> do 
            let author = EditAuthor {
                authorId = read . unpack $ head r :: Integer,
                description = r !! 1
            }
            res' <- Handler.editAuthor handle author
            case res' of 
                Right r' -> return $ responseLBS status200 [] ""
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: Query -> IO Response
delete query = do 
    let res = queryToList query ["author_id"]
    case res of 
        Right r -> do 
            res' <- Handler.deleteAuthor handle (read . unpack $ head r :: Integer)
            case res' of
                Right r' -> return $ responseLBS status200 [] ""
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

handle = Handler.Handle {
    Handler.create = manage . Db.create,
    Handler.get = manage . Db.get,
    Handler.delete = manage . Db.delete,
    Handler.edit = manage . Db.edit,
    Handler.doesExist = manage . Db.doesExist,
    Handler.doesUserExist = manage . UserDb.doesExist
}