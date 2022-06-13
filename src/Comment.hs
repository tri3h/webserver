{-# LANGUAGE OverloadedStrings #-}

module Comment where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Comment as Db
import Error (Error)
import qualified Handlers.Logger as Logger
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response)
import Types.Comment (CommentId (CommentId), CreateComment (..), commentsOnPage)
import Types.Limit (Offset (..))
import Types.PostComment (PostId (PostId))
import Types.User (Token, UserId (UserId))
import Utility (getInteger, getLimit, getOffset, getText, response200JSON, response201, response204, response400)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> Token -> IO Response
create logger pool query token = do
  let info = do
        postId <- getPostId query
        text <- getText query "text"
        Right CreateComment {cPostId = postId, cText = text, cToken = token}
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right comment -> do
      result <- withResource pool $ Db.create comment
      Logger.debug logger $ "Tried to create comment and got: " ++ show result
      return $ case result of
        Right _ -> response201
        Left l -> response400 l
    Left l -> return $ response400 l

get :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
get logger pool query = do
  let info = getPostId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right postId -> do
      let limit = getLimit query commentsOnPage
      let offset = getOffset query $ Offset 0
      result <- withResource pool $ Db.getEither postId limit offset
      Logger.debug logger $ "Tried to get comment and got: " ++ show result
      return $ case result of
        Right r -> response200JSON r
        Left l -> response400 l
    Left l -> return $ response400 l

delete :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
delete logger pool query = do
  let info = getCommentId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right commentId -> do
      result <- withResource pool $ Db.delete commentId
      Logger.debug logger $ "Tried to delete comment and got: " ++ show result
      return $ case result of
        Right _ -> response204
        Left l -> response400 l
    Left l -> return $ response400 l

getPostId :: QueryText -> Either Error PostId
getPostId query = PostId <$> getInteger query "post_id"

getUserId :: QueryText -> Either Error UserId
getUserId query = UserId <$> getInteger query "user_id"

getCommentId :: QueryText -> Either Error CommentId
getCommentId query = CommentId <$> getInteger query "comment_id"
