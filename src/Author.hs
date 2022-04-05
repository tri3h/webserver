{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Author where

import qualified Handler.Author as Handler
import Types.Author
    ( Author(AuthorToEdit, AuthorToCreate, userId, authorId,
             description) )
import qualified Database.Queries.Author as Db
import qualified Database.Queries.User as UserDb
import Database.Connection ( manage )
import Data.Aeson ( encode )
import Network.Wai ( Response, responseLBS )
import Network.HTTP.Types.Status
    ( status200, status201, status204, status400 )
import Network.HTTP.Types.Header ( hContentType )
import Network.HTTP.Types.URI ( QueryText )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.Text.Lazy as LazyText
import Data.Text ( Text, unpack )
import Data.ByteString.Lazy ( append )
import Utility ( getText, getInteger )

create :: QueryText -> IO Response
create query = do
    let info = do 
            userId <- getInteger query "user_id"
            description <- getText query "description"
            Right (userId, description) 
    case info of 
        Right (userId, description) ->  do
            let author = AuthorToCreate { .. }
            result <- Handler.create handle author
            case result of
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> return $ responseLBS status201 [] ""
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

get :: QueryText -> IO Response
get query = do 
    case getInteger query "author_id" of 
        Right authorId -> do 
            result <- Handler.get handle authorId
            case result of 
                Right r -> return $ responseLBS status200 
                    [(hContentType, "application/json")] $ encode r
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

edit :: QueryText -> IO Response
edit query = do 
    let info = do 
            authorId <- getInteger query "author_id"
            description <- getText query "description"
            Right $ AuthorToEdit { .. }
    case info of 
        Right author -> do 
            result <- Handler.edit handle author
            case result of 
                Right _ -> return $ responseLBS status201 [] ""
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: QueryText -> IO Response
delete query = do 
    case getInteger query "author_id" of 
        Right authorId -> do 
            result <- Handler.delete handle authorId
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
    Handler.hDoesUserExist = manage . UserDb.doesExist
}