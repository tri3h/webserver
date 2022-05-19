{-# LANGUAGE OverloadedStrings #-}

module Category where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Category as Db
import Error (Error)
import qualified Handlers.Category as Handler
import qualified Handlers.Logger as Logger
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response)
import Types.Category
  ( CategoryId (CategoryId),
    CreateCategory (..),
    Name (Name),
    ParentId,
  )
import Utility (getInteger, getMaybeInteger, getMaybeText, getText, response200JSON, response201, response204, response400)

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
      result <- withResource pool $ Db.create category
      Logger.debug logger $ "Tried to create category and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response201
    Left l -> return $ response400 l

get :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
get logger pool query = do
  let info = getMaybeCategoryId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Just categoryId -> do
      result <- withResource pool $ Db.get categoryId
      Logger.debug logger $ "Tried to get a category and got: " ++ show result
      return $ case result of
        Right r -> response200JSON r
        Left l -> response400 l
    Nothing -> do
      result <- withResource pool Db.getAll
      Logger.debug logger $ "Tried to get a list of categories and got: " ++ show result
      return $ response200JSON result

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
      return $ case result of
        Right _ -> response201
        Left l -> response400 l
    Left l -> return $ response400 l

delete :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
delete logger pool query = do
  let info = getCategoryId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right categoryId -> do
      result <- Handler.delete (handle pool) categoryId
      Logger.debug logger $ "Tried to delete category and got: " ++ show result
      return $ case result of
        Right _ -> response204
        Left l -> response400 l
    Left l -> return $ response400 l

getName :: QueryText -> Either Error Name
getName query = Name <$> getText query "name"

getMaybeName :: QueryText -> Maybe Name
getMaybeName query = Name <$> getMaybeText query "name"

getParentId :: QueryText -> Maybe ParentId
getParentId query = CategoryId <$> getMaybeInteger query "parent_id"

getCategoryId :: QueryText -> Either Error CategoryId
getCategoryId query = CategoryId <$> getInteger query "category_id"

getMaybeCategoryId :: QueryText -> Maybe CategoryId
getMaybeCategoryId query = CategoryId <$> getMaybeInteger query "category_id"

handle :: Pool Connection -> Handler.Handle IO
handle pool =
  let f = withResource pool
   in Handler.Handle
        { Handler.hDelete = f . Db.delete,
          Handler.hEditName = \a b -> f $ Db.editName a b,
          Handler.hEditParent = \a b -> f $ Db.editParent a b,
          Handler.hGetChildren = f . Db.getChildren
        }
