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
import Types.Config (Config (database))
import Types.Tag (Tag (Tag, name, tagId))
import Utility (getInteger, getText)

create :: Logger.Handle IO -> Config -> QueryText -> IO Response
create logger config query = do
  let info = getText query "name"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right name -> do
      result <- Handler.create (handle config) name
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

get :: Logger.Handle IO -> Config -> QueryText -> IO Response
get logger config query = do
  let info = getInteger query "tag_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right tagId -> do
      result <- Handler.get (handle config) tagId
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

edit :: Logger.Handle IO -> Config -> QueryText -> IO Response
edit logger config query = do
  let info = do
        name <- getText query "name"
        tagId <- getInteger query "tag_id"
        Right (tagId, name)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (tagId, name) -> do
      let tag = Tag {..}
      result <- Handler.edit (handle config) tag
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

delete :: Logger.Handle IO -> Config -> QueryText -> IO Response
delete logger config query = do
  let info = getInteger query "tag_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right tagId -> do
      result <- Handler.delete (handle config) tagId
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

handle :: Config -> Handler.Handle IO
handle config =
  let db = database config
   in Handler.Handle
        { Handler.hCreate = manage db . Db.create,
          Handler.hGet = manage db . Db.get,
          Handler.hDelete = manage db . Db.delete,
          Handler.hEdit = manage db . Db.edit,
          Handler.hDoesExist = manage db . Db.doesExist
        }
