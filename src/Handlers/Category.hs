{-# LANGUAGE OverloadedStrings #-}

module Handlers.Category where

import Error (Error, invalidParent, noDeleteHasChildren)
import qualified Handlers.Logger as Logger
import Network.HTTP.Types (QueryText)
import Network.Wai (Response)
import Types.Category
  ( CategoryId (CategoryId),
    CreateCategory (CreateCategory, cName, cParentId),
    GetCategory,
    Name (Name),
    ParentId,
    categoriesOnPage,
  )
import Types.Limit (Limit, Offset (Offset))
import Utility (getInteger, getLimit, getMaybeInteger, getMaybeText, getOffset, getText, response200JSON, response201, response204, response400)

data Handle m = Handle
  { hDelete :: CategoryId -> m (Either Error ()),
    hEditName :: CategoryId -> Name -> m (Either Error ()),
    hEditParent :: CategoryId -> ParentId -> m (Either Error ()),
    hGetChildren :: CategoryId -> m [CategoryId],
    hCreate :: CreateCategory -> m (Either Error ()),
    hGet :: CategoryId -> m (Either Error GetCategory),
    hGetAll :: Limit -> Offset -> m [GetCategory]
  }

create :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
create handle logger query = do
  let info = getName query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right name -> do
      let category =
            CreateCategory
              { cParentId = getParentId query,
                cName = name
              }
      result <- hCreate handle category
      Logger.debug logger $ "Tried to create category and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response201
    Left l -> return $ response400 l

get :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
get handle logger query = do
  let info = getMaybeCategoryId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Just categoryId -> do
      result <- hGet handle categoryId
      Logger.debug logger $ "Tried to get a category and got: " ++ show result
      return $ case result of
        Right r -> response200JSON r
        Left l -> response400 l
    Nothing -> do
      let limit = getLimit query categoriesOnPage
      let offset = getOffset query $ Offset 0
      result <- hGetAll handle limit offset
      Logger.debug logger $ "Tried to get a list of categories and got: " ++ show result
      return $ response200JSON result

edit :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
edit handle logger query = do
  let info = getCategoryId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right categoryId -> do
      let parentId = getParentId query
      let name = getMaybeName query
      result <- editCategory handle categoryId name parentId
      Logger.debug logger $ "Tried to edit category and got: " ++ show result
      return $ case result of
        Right _ -> response201
        Left l -> response400 l
    Left l -> return $ response400 l
  where
    editCategory :: Monad m => Handle m -> CategoryId -> Maybe Name -> Maybe ParentId -> m (Either Error ())
    editCategory h categId name parId = do
      resName <- case name of
        Nothing -> return $ Right ()
        Just n -> hEditName h categId n
      resParent <- case parId of
        Nothing -> return $ Right ()
        Just p -> do
          children <- hGetChildren h categId
          if p `notElem` children
            then hEditParent h categId p
            else return $ Left invalidParent
      return (resName >> resParent)

delete :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
delete handle logger query = do
  let info = getCategoryId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right categoryId -> do
      result <- deleteCategory handle categoryId
      Logger.debug logger $ "Tried to delete category and got: " ++ show result
      return $ case result of
        Right _ -> response204
        Left l -> response400 l
    Left l -> return $ response400 l
  where
    deleteCategory :: Monad m => Handle m -> CategoryId -> m (Either Error ())
    deleteCategory h categId = do
      children <- hGetChildren h categId
      if null children
        then hDelete h categId
        else return $ Left noDeleteHasChildren

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
