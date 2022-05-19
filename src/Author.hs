{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Author where

import Data.Aeson (encode)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Author as Db
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
import Types.Author
  ( AuthorId (AuthorId),
    CreateAuthor (..),
    Description (Description),
    EditAuthor (..),
  )
import Types.User (UserId (UserId))
import Utility (getInteger, getText)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
create logger pool query = do
  let info = do
        userId <- getUserId query
        description <- getDescription query
        Right (userId, description)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (cUserId, cDescription) -> do
      let author = CreateAuthor {..}
      result <- withResource pool $ Db.create author
      Logger.debug logger $ "Tried to create author and got: " ++ show result
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
  let info = getAuthorId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right authorId -> do
      result <- withResource pool $ Db.get authorId
      Logger.debug logger $ "Tried to get author and got: " ++ show result
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
    Left l -> return . responseLBS status400 [] $ encode l

edit :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
edit logger pool query = do
  let info = do
        eAuthorId <- getAuthorId query
        eDescription <- getDescription query
        Right $ EditAuthor {..}
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right author -> do
      result <- withResource pool $ Db.edit author
      Logger.debug logger $ "Tried to edit author and got: " ++ show result
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
  let info = getAuthorId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right authorId -> do
      result <- withResource pool $ Db.delete authorId
      Logger.debug logger $ "Tried to delete author and got: " ++ show result
      case result of
        Right _ -> return $ responseLBS status204 [] ""
        Left l ->
          return $
            responseLBS
              status400
              []
              $ encode l
    Left l -> return . responseLBS status400 [] $ encode l

getUserId :: QueryText -> Either Error UserId
getUserId query = UserId <$> getInteger query "user_id"

getDescription :: QueryText -> Either Error Description
getDescription query = Description <$> getText query "description"

getAuthorId :: QueryText -> Either Error AuthorId
getAuthorId query = AuthorId <$> getInteger query "author_id"
