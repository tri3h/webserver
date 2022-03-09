{-# LANGUAGE OverloadedStrings #-}
module Handler.Category where

import Types.Category
import Data.Text

data Handle m = Handle {
    create :: Category -> m Bool,
    get :: CategoryId -> m Category,
    delete :: CategoryId -> m Bool,
    edit :: Category -> m Bool,
    doesExist :: CategoryId -> m Bool,
    getParents :: CategoryId -> m [CategoryId],
    getChildren :: CategoryId -> m [CategoryId]
}

createCategory :: Monad m => Handle m -> Category -> m (Either Text Text)
createCategory handle cat = do 
        a <- case parentId cat of
                Nothing -> return True
                Just b -> doesExist handle b
        if a 
            then do 
            res <- create handle cat 
            if res 
                then return $ Right "Category was created"
                else return $ Left "Failed to create category"
        else return $ Left "Invalid parent id"

getCategory :: Monad m => Handle m -> CategoryId -> m (Either Text Category)
getCategory handle catId = do 
    a <- doesExist handle catId 
    if a
        then Right <$> get handle catId
        else return . Left $ "Such category doesn't exist"

deleteCategory :: Monad m => Handle m -> CategoryId -> m (Either Text ())
deleteCategory handle catId = do
        res <- delete handle catId
        if res 
            then return $ Right ()
            else return $ Left "Failed to delete category"

editCategory :: Monad m => Handle m -> Category -> m (Either Text ())
editCategory handle cat = do 
        a <- case parentId cat of
                Nothing -> return True
                Just b -> do 
                    doesExist <- doesExist handle b
                    children <- getChildren handle $ categoryId cat
                    return $ doesExist && (b `notElem` children)
        if a 
            then do 
            res <- edit handle cat 
            if res 
                then return $ Right ()
                else return $ Left "Failed to edit category"
        else return $ Left "Invalid parent id"
