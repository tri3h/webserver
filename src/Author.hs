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

create :: QueryText -> IO Response
create query = do
    let isAuthor = getInteger query "user_id" >>=
            \userId -> getText query "description" >>=
            \descr -> Right (userId, descr) 
    case isAuthor of 
        Right (userId, descr) ->  do
            let author = AuthorToCreate {
                userId = userId,
                description = descr
            }
            result <- Handler.createAuthor handle author
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right r -> return $ responseLBS status200 [] ""
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

get :: QueryText -> IO Response
get query = do 
    case getInteger query "author_id" of 
        Right authorId -> do 
            res' <- Handler.getAuthor handle authorId
            case res' of 
                Right r' -> return $ responseLBS status200 [(hContentType, "application/json")] $ encode r'
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

edit :: QueryText -> IO Response
edit query = do 
    let isAuthor = getInteger query "author_id" >>=
            \authorId -> getText query "description" >>=
            \descr -> Right $ AuthorToEdit {
                authorId = authorId,
                description = descr
            }
    case isAuthor of 
        Right author -> do 
            res' <- Handler.editAuthor handle author
            case res' of 
                Right r' -> return $ responseLBS status200 [] ""
                Left l' -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l'
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: QueryText -> IO Response
delete query = do 
    case getInteger query "author_id" of 
        Right authorId -> do 
            res' <- Handler.deleteAuthor handle authorId
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
    Handler.doesUserExist = manage . UserDb.doesExist
}