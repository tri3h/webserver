{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tag where

import Data.Aeson (encode)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Tag as Db
import Error (Error)
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
import Types.Tag (Name (Name), Tag (..), TagId (TagId))
import Utility (getInteger, getMaybeInteger, getText)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
create logger pool query = do
  let info = getName query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right name -> do
      result <- withResource pool $ Db.create name
      Logger.debug logger $ "Tried to create tag and got: " ++ show result
      case result of
        Left l ->
          return
            . responseLBS
              status400
              []
            $ encode l
        Right _ -> return $ responseLBS status201 [] ""
    Left l -> return . responseLBS status400 [] $ encode l

get :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
get logger pool query = do
  let info = getMaybeTagId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Just tagId -> do
      result <- withResource pool $ Db.get tagId
      Logger.debug logger $ "Tried to get a tag and got: " ++ show result
      case result of
        Right r ->
          return $
            responseLBS
              status200
              [(hContentType, "application/json")]
              $ encode r
        Left l ->
          return
            . responseLBS
              status400
              []
            $ encode l
    Nothing -> do
      result <- withResource pool Db.getAll
      Logger.debug logger $ "Tried to get a list of tags and got: " ++ show result
      return $
        responseLBS
          status200
          [(hContentType, "application/json")]
          $ encode result

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
      case result of
        Right _ -> return $ responseLBS status201 [] ""
        Left l ->
          return
            . responseLBS
              status400
              []
            $ encode l
    Left l -> return . responseLBS status400 [] $ encode l

delete :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
delete logger pool query = do
  let info = getTagId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right tagId -> do
      result <- withResource pool $ Db.delete tagId
      Logger.debug logger $ "Tried to delete tag and got: " ++ show result
      case result of
        Right _ -> return $ responseLBS status204 [] ""
        Left l ->
          return
            . responseLBS
              status400
              []
            $ encode l
    Left l -> return . responseLBS status400 [] $ encode l

getName :: QueryText -> Either Error Name
getName query = Name <$> getText query "name"

getTagId :: QueryText -> Either Error TagId
getTagId query = TagId <$> getInteger query "tag_id"

getMaybeTagId :: QueryText -> Maybe TagId
getMaybeTagId query = TagId <$> getMaybeInteger query "tag_id"
