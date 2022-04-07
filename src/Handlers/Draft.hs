{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Handlers.Draft(Handle(..), create, get, edit,
delete, publish, addMinorPhoto, deleteMinorPhoto, isAuthor, getAuthorIdByToken) where

import Data.Text (Text, unpack, words)
import Prelude hiding (words)
import Types.Draft
    ( Description,
      Draft (mainPhoto, minorPhoto, categoryId, authorId, tagId),
      DraftId,
      EditParams(eCategoryId, eTagId, eName, eDescription, eMainPhoto),
      Name, noDraftAuthor, userNotAuthor, noDeleteHasPost )
import Types.Image ( Image (Image, Link), ImageId, malformedImage )
import Types.Author(AuthorId, Author)
import Types.Category(CategoryId, noDeleteHasChildren)
import Types.Tag(TagId)
import Types.Post(PostId, Date)
import Types.User(Token)
import Network.HTTP.Types.URI ( Query )
import Data.Foldable ( forM_ )

data Handle m = Handle {
    hCreate :: Draft -> m (),
    hEditCategoryId :: DraftId -> CategoryId -> m (),
    hEditTagId :: DraftId -> [TagId] -> m (),
    hEditName :: DraftId -> Name -> m (),
    hEditDescription :: DraftId -> Description -> m (),
    hEditMainPhoto :: DraftId -> Image -> m (),
    hDelete :: DraftId -> m (),
    hAddMinorPhoto :: DraftId -> Image -> m (),
    hDeleteMinorPhoto :: DraftId -> ImageId -> m (),
    hIsAuthor :: Token -> m Bool,
    hHasPost :: DraftId -> m Bool,
    hGetCurrentDate :: m String,
    hGet :: DraftId -> m Draft,
    hGetAuthor :: DraftId -> m AuthorId,
    hPublish :: Draft -> String -> m (),
    hUpdate :: Draft -> m (),
    hGetAuthorIdByToken :: Token -> m AuthorId,
    hDoesExist :: DraftId -> m (Either Text ()),
    hDoesAuthorExist :: AuthorId -> m (Either Text ()),
    hDoesCategoryExist :: CategoryId -> m (Either Text ()),
    hDoesTagExist :: TagId -> m (Either Text ())
}

create :: Monad m => Handle m -> Draft -> m (Either Text ())
create handle draft = do
    authorExist <- hDoesAuthorExist handle $ authorId draft 
    categoryExist <- hDoesCategoryExist handle $ categoryId draft 
    tagExist <- foldl1 (>>) $ map (hDoesTagExist handle) $ tagId draft
    case authorExist >> categoryExist >> tagExist of 
        Right _ -> do 
            case mainPhoto draft of
                Image {} -> Right <$> hCreate handle draft
                _ -> return $ Left malformedImage
        Left l -> return $ Left l 

get :: Monad m => Handle m -> DraftId -> Token -> m (Either Text Draft)
get handle draftId token = do
    canAccess <- canAccess handle draftId token
    case canAccess of
        Left l -> return $ Left l
        Right _ -> do
            draft <- hGet handle draftId
            let isMinorPhotoCorrect = all (\case Link {} -> True; _ -> False) (minorPhoto draft)
            let isMainPhotoCorrect = case mainPhoto draft of Link {} -> True; _ -> False
            if isMinorPhotoCorrect && isMainPhotoCorrect
            then return $ Right draft
            else return $ Left malformedImage

edit :: Monad m => Handle m -> DraftId -> Token -> EditParams -> m (Either Text ())
edit handle draftId token params = do
    canAccess <- canAccess handle draftId token
    case canAccess of
        Left l -> return $ Left l
        Right _ -> do
            forM_ (eName params) (hEditName handle draftId)
            forM_ (eDescription params) (hEditDescription handle draftId)
            resCateg <- case eCategoryId params of 
                Nothing -> return $ Right ()
                Just categoryId -> editCategoryId handle draftId categoryId
            resTag <- case eTagId params of 
                Nothing -> return $ Right ()
                Just tagId -> editTagId handle draftId tagId
            resPhoto <- editPhoto handle draftId params
            return (resCateg >> resTag >> resPhoto)

editPhoto :: Monad m => Handle m -> DraftId -> EditParams -> m (Either Text ())
editPhoto handle draftId params = 
    case eMainPhoto params of
        Just x -> case x of
            Image {} -> Right <$> hEditMainPhoto handle draftId x
            _ -> return $ Left malformedImage
        _ -> return $ Right ()
            
editCategoryId :: Monad m => Handle m -> DraftId -> CategoryId -> m (Either Text ())
editCategoryId handle draftId categoryId = do 
    exist <- hDoesCategoryExist handle categoryId 
    case exist of 
        Left l -> return $ Left l 
        Right _ -> Right <$> hEditCategoryId handle draftId categoryId

editTagId :: Monad m => Handle m -> DraftId -> [TagId] -> m (Either Text ())
editTagId handle draftId tagId = do 
    exist <- foldl1 (>>) $ map (hDoesTagExist handle) tagId
    case exist of 
        Left l -> return $ Left l 
        Right _ -> Right <$> hEditTagId handle draftId tagId

delete :: Monad m => Handle m -> DraftId -> Token -> m (Either Text ())
delete handle draftId token = do
    canAccess <- canAccess handle draftId token
    hasPost <- hHasPost handle draftId
    let canDelete = if hasPost then Left noDeleteHasPost else Right ()
    case canAccess >> canDelete of
        Left l -> return $ Left l
        Right _ -> Right <$> hDelete handle draftId

publish :: Monad m => Handle m -> DraftId -> Token -> m (Either Text ())
publish handle draftId token = do
    canAccess <- canAccess handle draftId token
    case canAccess of
        Left l -> return $ Left l
        Right _ -> do
            hasPost <- hHasPost handle draftId
            draft <- hGet handle draftId
            if hasPost
            then Right <$> hUpdate handle draft
            else do
                date <- hGetCurrentDate handle
                Right <$> hPublish handle draft date

addMinorPhoto :: Monad m => Handle m -> DraftId -> Token -> Image -> m (Either Text ())
addMinorPhoto handle draftId token image = do
    case image of
        Image {} -> do
            canAccess <- canAccess handle draftId token
            case canAccess of
                Left l -> return $ Left l
                Right _ -> Right <$> hAddMinorPhoto handle draftId image
        _ -> return $ Left malformedImage

deleteMinorPhoto :: Monad m => Handle m -> DraftId -> Token -> ImageId -> m (Either Text ())
deleteMinorPhoto handle draftId token imageId = do
    canAccess <- canAccess handle draftId token
    case canAccess of
        Left l -> return $ Left l
        Right _ -> Right <$> hDeleteMinorPhoto handle draftId imageId

canAccess :: Monad m => Handle m -> DraftId -> Token -> m (Either Text ())
canAccess handle draftId token = do
    exist <- hDoesExist handle draftId
    case exist of
        Left l -> return $ Left l
        Right _ -> do
            author <- isAuthor handle draftId token
            case author of
                Left l -> return $ Left l
                Right _ -> return $ Right ()

isAuthor :: Monad m => Handle m -> DraftId -> Token -> m (Either Text ())
isAuthor handle draftId token = do
    author <- getAuthorIdByToken handle token
    case author of 
        Right authorId -> do 
            realAuthorId <- hGetAuthor handle draftId
            if authorId == realAuthorId
            then return $ Right ()
            else return $ Left noDraftAuthor
        Left l -> return $ Left l 

getAuthorIdByToken :: Monad m => Handle m -> Token -> m (Either Text AuthorId)
getAuthorIdByToken handle token = do 
    isAuthor <- hIsAuthor handle token
    if isAuthor
    then Right <$> hGetAuthorIdByToken handle token
    else return $ Left userNotAuthor
