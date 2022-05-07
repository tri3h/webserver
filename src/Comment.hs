{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Comment where

import Data.Aeson (encode)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Comment as Db
import qualified Database.Queries.Post as Db.Post
import Error (Error)
import qualified Handlers.Comment as Handler
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
import Types.Comment (CommentId (CommentId), CreateComment (..))
import Types.PostComment (PostId (PostId))
import Types.User (UserId (UserId))
import Utility (getInteger, getText)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
create logger pool query = do
  let info = do
        cPostId <- getPostId query
        cUserId <- getUserId query
        cText <- getText query "text"
        Right CreateComment {..}
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right comment -> do
      result <- Handler.create (handle pool) comment
      Logger.debug logger $ "Tried to create comment and got: " ++ show result
      case result of
        Right _ -> return $ responseLBS status201 [] ""
        Left l ->
          return
            . responseLBS
              status400
              []
            $ encode l
    Left l -> return . responseLBS status400 [] $ encode l

get :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
get logger pool query = do
  let info = getPostId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right postId -> do
      result <- Handler.get (handle pool) postId
      Logger.debug logger $ "Tried to get comment and got: " ++ show result
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

delete :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
delete logger pool query = do
  let info = getCommentId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right commentId -> do
      result <- Handler.delete (handle pool) commentId
      Logger.debug logger $ "Tried to delete comment and got: " ++ show result
      case result of
        Right _ -> return $ responseLBS status204 [] ""
        Left l ->
          return
            . responseLBS
              status400
              []
            $ encode l
    Left l -> return . responseLBS status400 [] $ encode l

getPostId :: QueryText -> Either Error PostId
getPostId query = PostId <$> getInteger query "post_id"

getUserId :: QueryText -> Either Error UserId
getUserId query = UserId <$> getInteger query "user_id"

getCommentId :: QueryText -> Either Error CommentId
getCommentId query = CommentId <$> getInteger query "comment_id"

handle :: Pool Connection -> Handler.Handle IO
handle pool =
  Handler.Handle
    { Handler.hGet = withResource pool . Db.get,
      Handler.hCreate = withResource pool . Db.create,
      Handler.hDelete = withResource pool . Db.delete,
      Handler.hDoesPostExist = withResource pool . Db.Post.doesExist,
      Handler.hDoesExist = withResource pool . Db.doesExist
    }
