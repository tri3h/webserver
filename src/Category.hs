{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Category where

import Data.Aeson (encode)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.Connection (manage)
import qualified Database.Queries.Category as Db
import qualified Handlers.Category as Handler
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
import Types.Category
  ( Category
      ( CategoryToCreate,
        name,
        parentId
      ),
  )
import Utility (getInteger, getMaybeInteger, getMaybeText, getText)

create :: Logger.Handle IO -> QueryText -> IO Response
create logger query = do
  let info = getText query "name"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right name -> do
      let category =
            CategoryToCreate
              { parentId = getMaybeInteger query "parent_id",
                ..
              }
      result <- Handler.create handle category
      Logger.debug logger $ "Tried to create category and got: " ++ show result
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
  let info = getInteger query "category_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right categoryId -> do
      result <- Handler.get handle categoryId
      Logger.debug logger $ "Tried to get category and got: " ++ show result
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
  let info = getInteger query "category_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right categoryId -> do
      let parentId = getMaybeInteger query "parent_id"
      let name = getMaybeText query "name"
      result <- Handler.edit handle categoryId name parentId
      Logger.debug logger $ "Tried to edit category and got: " ++ show result
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
  let info = getInteger query "category_id"
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right categoryId -> do
      result <- Handler.delete handle categoryId
      Logger.debug logger $ "Tried to delete category and got: " ++ show result
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
      Handler.hEditName = \a b -> manage $ Db.editName a b,
      Handler.hEditParent = \a b -> manage $ Db.editParent a b,
      Handler.hDoesExist = manage . Db.doesExist,
      Handler.hGetChildren = manage . Db.getChildren
    }
