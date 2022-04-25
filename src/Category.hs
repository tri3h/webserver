{-# LANGUAGE OverloadedStrings #-}

module Category where

import Data.Aeson (encode)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection)
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
  ( CategoryId (CategoryId),
    CreateCategory (..),
    Name (Name),
    ParentId,
  )
import Utility (getInteger, getMaybeInteger, getMaybeText, getText)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
create logger pool query = do
  let info = getName query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right name -> do
      let category =
            CreateCategory
              { cParentId = getParentId query,
                cName = name
              }
      result <- Handler.create (handle pool) category
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

get :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
get logger pool query = do
  let info = getCategoryId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right categoryId -> do
      result <- Handler.get (handle pool) categoryId
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

edit :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
edit logger pool query = do
  let info = getCategoryId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right categoryId -> do
      let parentId = getParentId query
      let name = getMaybeName query
      result <- Handler.edit (handle pool) categoryId name parentId
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

delete :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
delete logger pool query = do
  let info = getCategoryId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right categoryId -> do
      result <- Handler.delete (handle pool) categoryId
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

getName :: QueryText -> Either Text Name
getName query = Name <$> getText query "name"

getMaybeName :: QueryText -> Maybe Name
getMaybeName query = Name <$> getMaybeText query "name"

getParentId :: QueryText -> Maybe ParentId
getParentId query = CategoryId <$> getMaybeInteger query "parent_id"

getCategoryId :: QueryText -> Either Text CategoryId
getCategoryId query = CategoryId <$> getInteger query "category_id"

handle :: Pool Connection -> Handler.Handle IO
handle pool =
  Handler.Handle
    { Handler.hCreate = withResource pool . Db.create,
      Handler.hGet = withResource pool . Db.get,
      Handler.hDelete = withResource pool . Db.delete,
      Handler.hEditName = \a b -> withResource pool $ Db.editName a b,
      Handler.hEditParent = \a b -> withResource pool $ Db.editParent a b,
      Handler.hDoesExist = withResource pool . Db.doesExist,
      Handler.hGetChildren = withResource pool . Db.getChildren
    }
