{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tag where

import Data.Aeson (encode)
import Data.Pool (Pool, withResource)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Tag as Db
import qualified Handlers.Logger as Logger
import qualified Handlers.Tag as Handler
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status
  ( status200,
    status201,
    status204,
    status400,
  )
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response, responseLBS)
import Types.Tag (Name (Name), Tag (..), TagId (TagId))
import Utility (getInteger, getText)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
create logger pool query = do
  let info = getText query "name"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right name -> do
      result <- Handler.create (handle pool) $ Name name
      Logger.debug logger $ "Tried to create tag and got: " ++ show result
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
  let info = getInteger query "tag_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right tagId -> do
      result <- Handler.get (handle pool) $ TagId tagId
      Logger.debug logger $ "Tried to get tag and got: " ++ show result
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
        name <- Name <$> getText query "name"
        tagId <- TagId <$> getInteger query "tag_id"
        Right (tagId, name)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (tagId, name) -> do
      let tag = Tag {..}
      result <- Handler.edit (handle pool) tag
      Logger.debug logger $ "Tried to edit tag and got: " ++ show result
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
  let info = getInteger query "tag_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right tagId -> do
      result <- Handler.delete (handle pool) $ TagId tagId
      Logger.debug logger $ "Tried to delete tag and got: " ++ show result
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

handle :: Pool Connection -> Handler.Handle IO
handle pool =
  Handler.Handle
    { Handler.hCreate = withResource pool . Db.create,
      Handler.hGet = withResource pool . Db.get,
      Handler.hDelete = withResource pool . Db.delete,
      Handler.hEdit = withResource pool . Db.edit,
      Handler.hDoesExist = withResource pool . Db.doesExist
    }
