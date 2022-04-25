{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Author where

import Data.Aeson (encode)
import Data.Pool (Pool, withResource)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Author as Db
import qualified Database.Queries.User as UserDb
import qualified Handlers.Author as Handler
import qualified Handlers.Logger as Logger
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status
  ( status200,
    status201,
    status204,
    status400,
  )
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response, responseLBS)
import Types.Author
  ( Description (Description),
    AuthorId (AuthorId),
    CreateAuthor (..),
    EditAuthor (..),
  )
import Types.User (UserId (UserId))
import Utility (getInteger, getText)
import Data.Text (Text)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
create logger pool query = do
  let info = do
        userId <- getUserId query
        description <- getDescription query
        Right (userId, description)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (cUserId, cDescription) -> do
      let author = CreateAuthor {..}
      result <- Handler.create (handle pool) author
      Logger.debug logger $ "Tried to create author and got: " ++ show result
      case result of
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
        Right _ -> return $ responseLBS status201 [] ""
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

get :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
get logger pool query = do
  let info = getAuthorId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right authorId -> do
      result <- Handler.get (handle pool) authorId
      Logger.debug logger $ "Tried to get author and got: " ++ show result
      case result of
        Right r ->
          return $
            responseLBS
              status200
              [(hContentType, "application/json")]
              $ encode r
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

edit :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
edit logger pool query = do
  let info = do
        eAuthorId <- getAuthorId query
        eDescription <- getDescription query
        Right $ EditAuthor {..}
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right author -> do
      result <- Handler.edit (handle pool) author
      Logger.debug logger $ "Tried to edit author and got: " ++ show result
      case result of
        Right _ -> return $ responseLBS status201 [] ""
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

delete :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
delete logger pool query = do
  let info = getAuthorId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right authorId -> do
      result <- Handler.delete (handle pool) authorId
      Logger.debug logger $ "Tried to delete author and got: " ++ show result
      case result of
        Right _ -> return $ responseLBS status204 [] ""
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l

getUserId :: QueryText -> Either Text UserId
getUserId query = UserId <$> getInteger query "user_id"

getDescription :: QueryText -> Either Text Description
getDescription query = Description <$> getText query "description"

getAuthorId :: QueryText -> Either Text AuthorId
getAuthorId query = AuthorId <$> getInteger query "author_id"

handle :: Pool Connection -> Handler.Handle IO
handle pool =
  Handler.Handle
    { Handler.hCreate = withResource pool . Db.create,
      Handler.hGet = withResource pool . Db.get,
      Handler.hDelete = withResource pool . Db.delete,
      Handler.hEdit = withResource pool . Db.edit,
      Handler.hDoesExist = withResource pool . Db.doesExist,
      Handler.hDoesUserExist = withResource pool . UserDb.doesExist
    }
