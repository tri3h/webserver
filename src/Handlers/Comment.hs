{-# LANGUAGE OverloadedStrings #-}

module Handlers.Comment where

import Error (Error)
import qualified Handlers.Logger as Logger
import Network.HTTP.Types (QueryText)
import Network.Wai (Response)
import Types.Comment
  ( CommentId (CommentId),
    CreateComment (CreateComment, cPostId, cText, cToken),
    GetComment,
    commentsOnPage,
  )
import Types.Limit (Limit, Offset (Offset))
import Types.PostComment (PostId (PostId))
import Types.User (Token, UserId (UserId))
import Utility (getInteger, getLimit, getOffset, getText, response200JSON, response201, response204, response400)

data Handle m = Handle
  { hCreate :: CreateComment -> m (Either Error ()),
    hGetEither :: PostId -> Limit -> Offset -> m (Either Error [GetComment]),
    hDelete :: CommentId -> m (Either Error ())
  }

create :: Monad m => Handle m -> Logger.Handle m -> QueryText -> Token -> m Response
create handle logger query token = do
  let info = do
        postId <- getPostId query
        text <- getText query "text"
        Right CreateComment {cPostId = postId, cText = text, cToken = token}
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right comment -> do
      result <- hCreate handle comment
      Logger.debug logger $ "Tried to create comment and got: " ++ show result
      return $ case result of
        Right _ -> response201
        Left l -> response400 l
    Left l -> return $ response400 l

get :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
get handle logger query = do
  let info = getPostId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right postId -> do
      let limit = getLimit query commentsOnPage
      let offset = getOffset query $ Offset 0
      result <- hGetEither handle postId limit offset
      Logger.debug logger $ "Tried to get comment and got: " ++ show result
      return $ case result of
        Right r -> response200JSON r
        Left l -> response400 l
    Left l -> return $ response400 l

delete :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
delete handle logger query = do
  let info = getCommentId query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Right commentId -> do
      result <- hDelete handle commentId
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
