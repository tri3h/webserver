{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Author where

import Data.Aeson (encode)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.Connection (manage)
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
  ( Author
      ( AuthorToCreate,
        AuthorToEdit,
        authorId,
        description,
        userId
      ),
  )
import Types.Config (Config (database))
import Utility (getInteger, getText)

create :: Logger.Handle IO -> Config -> QueryText -> IO Response
create logger config query = do
  let info = do
        userId <- getInteger query "user_id"
        description <- getText query "description"
        Right (userId, description)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (userId, description) -> do
      let author = AuthorToCreate {..}
      result <- Handler.create (handle config) author
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

get :: Logger.Handle IO -> Config -> QueryText -> IO Response
get logger config query = do
  let info = getInteger query "author_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right authorId -> do
      result <- Handler.get (handle config) authorId
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

edit :: Logger.Handle IO -> Config -> QueryText -> IO Response
edit logger config query = do
  let info = do
        authorId <- getInteger query "author_id"
        description <- getText query "description"
        Right $ AuthorToEdit {..}
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right author -> do
      result <- Handler.edit (handle config) author
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

delete :: Logger.Handle IO -> Config -> QueryText -> IO Response
delete logger config query = do
  let info = getInteger query "author_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right authorId -> do
      result <- Handler.delete (handle config) authorId
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

handle :: Config -> Handler.Handle IO
handle config =
  let db = database config
   in Handler.Handle
        { Handler.hCreate = manage db . Db.create,
          Handler.hGet = manage db . Db.get,
          Handler.hDelete = manage db . Db.delete,
          Handler.hEdit = manage db . Db.edit,
          Handler.hDoesExist = manage db . Db.doesExist,
          Handler.hDoesUserExist = manage db . UserDb.doesExist
        }
