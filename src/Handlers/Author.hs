{-# LANGUAGE OverloadedStrings #-}

module Handlers.Author where

import Error (Error)
import qualified Handlers.Logger as Logger
import Network.HTTP.Types (QueryText)
import Network.Wai (Response)
import Types.Author
  ( AuthorId (AuthorId),
    CreateAuthor (CreateAuthor, cDescription, cUserId),
    Description (Description),
    EditAuthor (EditAuthor, eAuthorId, eDescription),
    GetAuthor,
  )
import Types.User (UserId (UserId))
import Utility (getInteger, getText, response200JSON, response201, response204, response400)

data Handle m = Handle
  { hCreate :: CreateAuthor -> m (Either Error ()),
    hGet :: AuthorId -> m (Either Error GetAuthor),
    hEdit :: EditAuthor -> m (Either Error ()),
    hDelete :: AuthorId -> m (Either Error ())
  }

create :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
create handle logger query = do
  let info = do
        userId <- getUserId query
        description <- getDescription query
        Right (userId, description)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right (userId, description) -> do
      let author = CreateAuthor {cUserId = userId, cDescription = description}
      result <- hCreate handle author
      Logger.debug logger $ "Tried to create author and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response201
    Left l -> return $ response400 l

get :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
get handle logger query = do
  let info = getAuthorId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right authorId -> do
      result <- hGet handle authorId
      Logger.debug logger $ "Tried to get author and got: " ++ show result
      return $ case result of
        Right r -> response200JSON r
        Left l -> response400 l
    Left l -> return $ response400 l

edit :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
edit handle logger query = do
  let info = do
        authorId <- getAuthorId query
        description <- getDescription query
        Right $ EditAuthor {eAuthorId = authorId, eDescription = description}
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right author -> do
      result <- hEdit handle author
      Logger.debug logger $ "Tried to edit author and got: " ++ show result
      return $ case result of
        Right _ -> response201
        Left l -> response400 l
    Left l -> return $ response400 l

delete :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
delete handle logger query = do
  let info = getAuthorId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right authorId -> do
      result <- hDelete handle authorId
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
