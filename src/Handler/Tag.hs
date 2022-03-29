{-# LANGUAGE OverloadedStrings #-}
module Handler.Tag where

import Types.Tag
import Data.Text

data Handle m = Handle {
    create :: Name -> m Bool,
    get :: TagId -> m Tag,
    delete :: TagId -> m Bool,
    edit :: Tag -> m Bool,
    doesExist :: TagId -> m Bool
}

createTag :: Monad m => Handle m -> Name -> m (Either Text Text)
createTag handle name = do 
        res <- create handle name
        if res
            then return $ Right "Tag was created"
            else return $ Left "Failed to create tag"

getTag :: Monad m => Handle m -> TagId -> m (Either Text Tag)
getTag handle tagId = do 
    exist <- doesExist handle tagId
    if exist
        then Right <$> get handle tagId
        else return $ Left "Tag with such id doesn't exist"

editTag :: Monad m => Handle m -> Tag -> m (Either Text ())
editTag handle tag =  do 
    exist <- doesExist handle $ tagId tag
    if exist 
        then do 
            res <- edit handle tag 
            if res 
                then return $ Right ()
                else return $ Left "Failed to edit tag"
        else return $ Left "Tag with such id doesn't exist"

deleteTag :: Monad m => Handle m -> TagId -> m (Either Text ())
deleteTag handle tagId = do
    exist <- doesExist handle tagId
    if exist 
        then do 
            res <- delete handle tagId
            if res 
                then return $ Right ()
                else return $ Left "Failed to delete tag"
        else return $ Left "Tag with such id doesn't exist"