{-# LANGUAGE OverloadedStrings #-}
module Handler.Category where

import Types.Category
    ( Category(parentId, categoryId, Category, CategoryToCreate), CategoryId, invalidParent, malformedCategory, impossibleDelete )
import Data.Text ( Text )

data Handle m = Handle {
    hCreate :: Category -> m (),
    hGet :: CategoryId -> m Category,
    hDelete :: CategoryId -> m (),
    hEdit :: Category -> m (),
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
            else return $ Left impossibleDelete
        Left l -> return $ Left l

edit :: Monad m => Handle m -> Category -> m (Either Text ())
edit handle categ = do
    canEdit <- canEdit handle categ
    case canEdit of
        Right _ -> do
            isParentCorrect <- do
                case parentId categ of
                    Nothing -> return True
                    Just b -> do
                        existParent <- hDoesExist handle b
                        let doesExist = case existParent of Right _ -> True; Left _ -> False
                        children <- hGetChildren handle $ categoryId categ
                        return $ doesExist && (b `notElem` children)
            if isParentCorrect
            then Right <$> hEdit handle categ
            else return $ Left invalidParent
        Left l -> return $ Left l
    where canEdit :: Monad m => Handle m -> Category -> m (Either Text ())
          canEdit handle categ =
            let isFormatCorrect = case categ of Category {} -> True; _ -> False
            in if isFormatCorrect
                then hDoesExist handle $ categoryId categ
                else return $ Left malformedCategory

          