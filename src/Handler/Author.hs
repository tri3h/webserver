{-# LANGUAGE OverloadedStrings #-}
module Handler.Author where

import Types.Author
import Types.User (UserId)
import Data.Text

data Handle m = Handle {
    create :: Author -> m Bool,
    get :: AuthorId -> m Author,
    delete :: AuthorId -> m Bool,
    edit :: Author -> m Bool,
    doesExist :: AuthorId -> m Bool,
    doesUserExist :: UserId -> m Bool
}

createAuthor :: Monad m => Handle m -> Author -> m (Either Text Text)
createAuthor handle author = do
    exist <- doesUserExist handle $ userId author
    if exist
        then do
            res <- create handle author
            if res
                then return $ Right "Author was created"
                else return $ Left "Author was not created"
        else return $ Left "User with such id doesn't exist"

getAuthor :: Monad m => Handle m -> AuthorId -> m (Either Text Author)
getAuthor handle authorId = do
    exist <- doesExist handle authorId
    if exist
        then Right <$> get handle authorId
        else return $ Left "Author with such id doesn't exist"

deleteAuthor :: Monad m => Handle m -> AuthorId -> m (Either Text ())
deleteAuthor handle authorId = do
    exist <- doesExist handle authorId
    if exist 
        then do 
            res <- delete handle authorId
            if res 
                then return $ Right ()
                else return $ Left "Failed to delete author"
        else return $ Left "Author with such id doesn't exist"

editAuthor :: Monad m => Handle m -> Author -> m (Either Text ())
editAuthor handle author = do 
    exist <- doesExist handle $ authorId author
    if exist 
        then do 
            res <- edit handle author 
            if res 
                then return $ Right ()
                else return $ Left "Failed to edit author"
        else return $ Left "Author with such id doesn't exist"