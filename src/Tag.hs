{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tag where

import Data.Aeson (encode)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.Connection (manage)
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
import Types.Tag (Tag (Tag, name, tagId))
import Utility (getInteger, getText)

create :: Logger.Handle IO -> QueryText -> IO Response
create logger query = do
  let info = getText query "name"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right name -> do
      result <- Handler.create handle name
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

get :: Logger.Handle IO -> QueryText -> IO Response
get logger query = do
  let info = getInteger query "tag_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right tagId -> do
      result <- Handler.get handle tagId
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

edit :: Logger.Handle IO -> QueryText -> IO Response
edit logger query = do
  let info = do
        name <- getText query "name"
        tagId <- getInteger query "tag_id"
        Right (tagId, name)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (tagId, name) -> do
      let tag = Tag {..}
      result <- Handler.edit handle tag
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

delete :: Logger.Handle IO -> QueryText -> IO Response
delete logger query = do
  let info = getInteger query "tag_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right tagId -> do
      result <- Handler.delete handle tagId
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

handle :: Handler.Handle IO
handle =
  Handler.Handle
    { Handler.hCreate = manage . Db.create,
      Handler.hGet = manage . Db.get,
      Handler.hDelete = manage . Db.delete,
      Handler.hEdit = manage . Db.edit,
      Handler.hDoesExist = manage . Db.doesExist
    }
