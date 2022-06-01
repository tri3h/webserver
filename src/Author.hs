{-# LANGUAGE OverloadedStrings #-}

module Author where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Author as Db
import Error (Error)
import qualified Handlers.Logger as Logger
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response)
import Types.Author
  ( AuthorId (AuthorId),
    CreateAuthor (..),
    Description (Description),
    EditAuthor (..),
  )
import Types.User (UserId (UserId))
import Utility (getInteger, getText, response200JSON, response201, response204, response400)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
create logger pool query = do
  let info = do
        userId <- getUserId query
        description <- getDescription query
        Right (userId, description)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (userId, description) -> do
      let author = CreateAuthor {cUserId = userId, cDescription = description}
      result <- withResource pool $ Db.create author
      Logger.debug logger $ "Tried to create author and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response201
    Left l -> return $ response400 l

get :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
get logger pool query = do
  let info = getAuthorId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right authorId -> do
      result <- withResource pool $ Db.get authorId
      Logger.debug logger $ "Tried to get author and got: " ++ show result
      return $ case result of
        Right r -> response200JSON r
        Left l -> response400 l
    Left l -> return $ response400 l

edit :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
edit logger pool query = do
  let info = do
        authorId <- getAuthorId query
        description <- getDescription query
        Right $ EditAuthor {eAuthorId = authorId, eDescription = description}
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right author -> do
      result <- withResource pool $ Db.edit author
      Logger.debug logger $ "Tried to edit author and got: " ++ show result
      return $ case result of
        Right _ -> response201
        Left l -> response400 l
    Left l -> return $ response400 l

delete :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
delete logger pool query = do
  let info = getAuthorId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right authorId -> do
      result <- withResource pool $ Db.delete authorId
      Logger.debug logger $ "Tried to delete author and got: " ++ show result
      return $ case result of
        Right _ -> response204
        Left l -> response400 l
    Left l -> return $ response400 l

getUserId :: QueryText -> Either Error UserId
getUserId query = UserId <$> getInteger query "user_id"

getDescription :: QueryText -> Either Error Description
getDescription query = Description <$> getText query "description"

getAuthorId :: QueryText -> Either Error AuthorId
getAuthorId query = AuthorId <$> getInteger query "author_id"
