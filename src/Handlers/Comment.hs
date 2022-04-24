module Handlers.Comment where

import Data.Text (Text)
import Types.Comment (CommentId, CreateComment (cPostId), GetComment)
import Types.PostComment (PostId)

data Handle m = Handle
  { hGet :: PostId -> m [GetComment],
    hCreate :: CreateComment -> m (),
    hDelete :: CommentId -> m (),
    hDoesExist :: CommentId -> m (Either Text ()),
    hDoesPostExist :: PostId -> m (Either Text ())
  }

create :: Monad m => Handle m -> CreateComment -> m (Either Text ())
create handle comm = do
  postExist <- hDoesPostExist handle $ cPostId comm
  case postExist of
    Right _ -> Right <$> hCreate handle comm
    Left l -> return $ Left l

get :: Monad m => Handle m -> PostId -> m (Either Text [GetComment])
get handle pId = do
  postExist <- hDoesPostExist handle pId
  case postExist of
    Right _ -> Right <$> hGet handle pId
    Left l -> return $ Left l

delete :: Monad m => Handle m -> CommentId -> m (Either Text ())
delete handle commId = do
  exist <- hDoesExist handle commId
  case exist of
    Right _ -> Right <$> hDelete handle commId
    Left l -> return $ Left l
