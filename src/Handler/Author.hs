{-# LANGUAGE OverloadedStrings #-}
module Handler.Author where

import Types.Author ( Author(userId, authorId, AuthorToCreate, 
    AuthorToGet, AuthorToEdit), AuthorId, malformedAuthor )
import Types.User ( UserId )
import Data.Text ( Text )

data Handle m = Handle {
    hCreate :: Author -> m (),
    hGet :: AuthorId -> m Author,
    hDelete :: AuthorId -> m (),
    hEdit :: Author -> m (),
    hDoesExist :: AuthorId -> m (Either Text ()),
    hDoesUserExist :: UserId -> m (Either Text ())
}

create :: Monad m => Handle m -> Author -> m (Either Text ())
create handle author = do
    let isFormatCorrect = case author of AuthorToCreate {} -> True; _ -> False 
    if isFormatCorrect
    then do
        exist <- hDoesUserExist handle $ userId author
        case exist of 
            Right () -> do
                hCreate handle author
                return $ Right ()
            Left l -> return $ Left l
    else return $ Left malformedAuthor

get :: Monad m => Handle m -> AuthorId -> m (Either Text Author)
get handle authorId = do
    exist <-hDoesExist handle authorId
    case exist of 
        Right _ -> do 
            author <- hGet handle authorId
            let isFormatCorrect = case author of AuthorToGet {} -> True; _ -> False 
            if isFormatCorrect
            then return $ Right author
            else return $ Left malformedAuthor
        Left l -> return $ Left l

delete :: Monad m => Handle m -> AuthorId -> m (Either Text ())
delete handle authorId = do
    exist <- hDoesExist handle authorId
    case exist of
        Right () -> do 
            hDelete handle authorId
            return $ Right ()
        Left l -> return $ Left l

edit :: Monad m => Handle m -> Author -> m (Either Text ())
edit handle author = do 
    let isFormatCorrect = case author of AuthorToEdit {} -> True; _ -> False 
    if isFormatCorrect
    then do 
        exist <- hDoesExist handle $ authorId author
        case exist of
            Right () -> do 
                hEdit handle author 
                return $ Right ()
            Left l -> return $ Left l
    else return $ Left malformedAuthor

