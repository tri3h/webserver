{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tag where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Tag as Db
import Error (Error)
import qualified Handlers.Logger as Logger
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response)
import Types.Limit (Offset (Offset))
import Types.Tag (Name (Name), Tag (..), TagId (TagId), tagsOnPage)
import Utility (getInteger, getLimit, getMaybeInteger, getOffset, getText, response200JSON, response201, response204, response400)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
create logger pool query = do
  let info = getName query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right name -> do
      result <- withResource pool $ Db.create name
      Logger.debug logger $ "Tried to create tag and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response201
    Left l -> return $ response400 l

get :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
get logger pool query = do
  let info = getMaybeTagId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Just tagId -> do
      result <- withResource pool $ Db.get tagId
      Logger.debug logger $ "Tried to get a tag and got: " ++ show result
      return $ case result of
        Right r -> response200JSON r
        Left l -> response400 l
    Nothing -> do
      let limit = getLimit query tagsOnPage
      let offset = getOffset query $ Offset 0
      result <- withResource pool $ Db.getAll limit offset
      Logger.debug logger $ "Tried to get a list of tags and got: " ++ show result
      return $ response200JSON result

edit :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
edit logger pool query = do
  let info = do
        name <- getName query
        tagId <- getTagId query
        Right (tagId, name)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (tagId, name) -> do
      let tag = Tag {..}
      result <- withResource pool $ Db.edit tag
      Logger.debug logger $ "Tried to edit tag and got: " ++ show result
      return $ case result of
        Right _ -> response201
        Left l -> response400 l
    Left l -> return $ response400 l

delete :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
delete logger pool query = do
  let info = getTagId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right tagId -> do
      result <- withResource pool $ Db.delete tagId
      Logger.debug logger $ "Tried to delete tag and got: " ++ show result
      return $ case result of
        Right _ -> response204
        Left l -> response400 l
    Left l -> return $ response400 l

getName :: QueryText -> Either Error Name
getName query = Name <$> getText query "name"

getTagId :: QueryText -> Either Error TagId
getTagId query = TagId <$> getInteger query "tag_id"

getMaybeTagId :: QueryText -> Maybe TagId
getMaybeTagId query = TagId <$> getMaybeInteger query "tag_id"
