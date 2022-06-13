{-# LANGUAGE OverloadedStrings #-}

module Handlers.Tag where

import Error (Error (..))
import qualified Handlers.Logger as Logger
import Network.HTTP.Types (QueryText)
import Network.Wai (Response)
import Types.Limit (Limit (..), Offset (..))
import qualified Types.Tag as T
import Utility (getInteger, getLimit, getMaybeInteger, getOffset, getText, response200JSON, response201, response204, response400)

data Handle m = Handle
  { hCreate :: T.Name -> m (Either Error ()),
    hGet :: T.TagId -> m (Either Error T.Tag),
    hGetAll :: Limit -> Offset -> m [T.Tag],
    hEdit :: T.Tag -> m (Either Error ()),
    hDelete :: T.TagId -> m (Either Error ())
  }

create :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
create handle logger query = do
  let info = getName query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right name -> do
      result <- hCreate handle name
      Logger.debug logger $ "Tried to create tag and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response201
    Left l -> return $ response400 l

get :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
get handle logger query = do
  let info = getMaybeTagId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Just tagId -> do
      result <- hGet handle tagId
      Logger.debug logger $ "Tried to get a tag and got: " ++ show result
      return $ case result of
        Right r -> response200JSON r
        Left l -> response400 l
    Nothing -> do
      let limit = getLimit query T.tagsOnPage
      let offset = getOffset query $ Offset 0
      result <- hGetAll handle limit offset
      Logger.debug logger $ "Tried to get a list of tags and got: " ++ show result
      return $ response200JSON result

edit :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
edit handle logger query = do
  let info = do
        name <- getName query
        tagId <- getTagId query
        Right (tagId, name)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (tagId, name) -> do
      let tag = T.Tag {T.tagId = tagId, T.name = name}
      result <- hEdit handle tag
      Logger.debug logger $ "Tried to edit tag and got: " ++ show result
      return $ case result of
        Right _ -> response201
        Left l -> response400 l
    Left l -> return $ response400 l

delete :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
delete handle logger query = do
  let info = getTagId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right tagId -> do
      result <- hDelete handle tagId
      Logger.debug logger $ "Tried to delete tag and got: " ++ show result
      return $ case result of
        Right _ -> response204
        Left l -> response400 l
    Left l -> return $ response400 l

getName :: QueryText -> Either Error T.Name
getName query = T.Name <$> getText query "name"

getTagId :: QueryText -> Either Error T.TagId
getTagId query = T.TagId <$> getInteger query "tag_id"

getMaybeTagId :: QueryText -> Maybe T.TagId
getMaybeTagId query = T.TagId <$> getMaybeInteger query "tag_id"
