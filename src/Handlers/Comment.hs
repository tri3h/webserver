{-# LANGUAGE LambdaCase #-}

module Handlers.Comment where

import Data.Text (Text)
import Types.Comment (Comment (CommentToCreate, CommentToGet, postId), CommentId, malformedComment)
import Types.Post (PostId)

data Handle m = Handle
  { hGet :: PostId -> m [Comment],
    hCreate :: Comment -> m (),
    hDelete :: CommentId -> m (),
    hDoesExist :: CommentId -> m (Either Text ()),
    hDoesPostExist :: PostId -> m (Either Text ())
  }

create :: Monad m => Handle m -> Comment -> m (Either Text ())
create handle comm = do
  let isFormatCorrect = case comm of CommentToCreate {} -> True; _ -> False
  if isFormatCorrect
    then do
      postExist <- hDoesPostExist handle $ postId comm
      case postExist of
        Right _ -> Right <$> hCreate handle comm
        Left l -> return $ Left l
    else return $ Left malformedComment

get :: Monad m => Handle m -> PostId -> m (Either Text [Comment])
get handle pId = do
  postExist <- hDoesPostExist handle pId
  case postExist of
    Right _ -> do
      comments <- hGet handle pId
      let isFormatCorrect = all (\case CommentToGet {} -> True; _ -> False) comments
      if isFormatCorrect
        then return $ Right comments
        else return $ Left malformedComment
    Left l -> return $ Left l

delete :: Monad m => Handle m -> CommentId -> m (Either Text ())
delete handle commId = do
  exist <- hDoesExist handle commId
  case exist of
    Right _ -> Right <$> hDelete handle commId
    Left l -> return $ Left l
