{-# LANGUAGE OverloadedStrings #-}
module Handler.Comment where

import Types.Comment
import Types.Post (PostId)
import Data.Text (Text)

data Handle m = Handle {
    hGet :: PostId -> m [Comment],
    hCreate :: Comment -> m Bool,
    hDelete :: CommentId -> m Bool
}

get :: Monad m => Handle m -> PostId -> m (Either Text [Comment])
get handle postId = do 
    comments <- hGet handle postId 
    if null comments 
        then return $ Left "No comments found"
        else return $ Right comments

create :: Monad m => Handle m -> Comment -> m (Either Text ())
create handle comm = do 
    isCreated <- hCreate handle comm
    if isCreated
        then return $ Right ()
        else return $ Left "Failed to create comment"

delete :: Monad m => Handle m -> CommentId -> m (Either Text ())
delete handle commId = do 
    isDeleted <- hDelete handle commId 
    if isDeleted 
        then return $ Right ()
        else return $ Left "Failed to delete comment"
