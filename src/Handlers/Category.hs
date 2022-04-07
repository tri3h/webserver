{-# LANGUAGE OverloadedStrings #-}
module Handlers.Category where

import Types.Category
    ( Category(parentId, categoryId, Category, CategoryToCreate), CategoryId, invalidParent, malformedCategory, noDeleteHasChildren, Name, ParentId )
import Data.Text ( Text )

data Handle m = Handle {
    hCreate :: Category -> m (),
    hGet :: CategoryId -> m Category,
    hDelete :: CategoryId -> m (),
    hEditName :: CategoryId -> Name -> m (),
    hEditParent :: CategoryId -> ParentId -> m (),
    hDoesExist :: CategoryId -> m (Either Text ()),
    hGetChildren :: CategoryId -> m [CategoryId]
}

create :: Monad m => Handle m -> Category -> m (Either Text ())
create handle categ = do
    let isFormatCorrect = case categ of CategoryToCreate {} -> True; _ -> False
    if isFormatCorrect
    then do
        correct <- isParentCorrect handle categ
        if correct
        then Right <$> hCreate handle categ
        else return $ Left invalidParent
    else return $ Left malformedCategory
    where isParentCorrect handle categ =
            case parentId categ of
                Nothing -> return True
                Just b -> do
                    exist <- hDoesExist handle b
                    return $ case exist of
                        Right () -> True
                        _ -> False

get :: Monad m => Handle m -> CategoryId -> m (Either Text Category)
get handle categId = do
    exist <- hDoesExist handle categId
    case exist of
        Right _ -> do
            categ <- hGet handle categId
            let isFormatCorrect = case categ of Category {} -> True; _ -> False
            if isFormatCorrect
            then return $ Right categ
            else return $ Left malformedCategory
        Left l -> return $ Left l

delete :: Monad m => Handle m -> CategoryId -> m (Either Text ())
delete handle categId = do
    exist <- hDoesExist handle categId
    case exist of
        Right _ -> do
            children <- hGetChildren handle categId
            if null children 
            then do
                Right <$> hDelete handle categId
            else return $ Left noDeleteHasChildren
        Left l -> return $ Left l

edit :: Monad m => Handle m -> CategoryId -> Maybe Name -> Maybe ParentId -> m (Either Text ())
edit handle categoryId name parentId = do
    doesExist <- hDoesExist handle categoryId
    case doesExist of
        Right _ -> do
            case parentId of
                Nothing -> editName handle categoryId name
                Just p -> editParent handle categoryId p >> editName handle categoryId name
        Left l -> return $ Left l
    where editName handle categoryId name = case name of 
                    Nothing -> return $ Right ()
                    Just n -> Right <$> hEditName handle categoryId n
          editParent handle categoryId p = do
                    existParent <- hDoesExist handle p
                    let doesExist = case existParent of Right _ -> True; Left _ -> False
                    children <- hGetChildren handle categoryId
                    let isParentCorrect = doesExist && (p `notElem` children)
                    if isParentCorrect
                    then Right <$> hEditParent handle categoryId p
                    else return $ Left invalidParent

          