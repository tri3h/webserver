{-# LANGUAGE OverloadedStrings #-}
module Handler.Draft(Handle(..), create, get, edit, delete, publish, addMinorPhoto, deleteMinorPhoto) where

import Prelude hiding (words)
import Data.Text (Text, unpack, words)
import Types.Draft
import Types.Image
import Types.Author(AuthorId)
import Types.Category(CategoryId)
import Types.Tag(TagId)
import Types.Post(PostId, Date)
import Types.User(Token)
import Network.HTTP.Types.URI ( Query )
import Utility
import Control.Monad
import Data.Text.Encoding (decodeUtf8)

data Handle m = Handle {
    hCreate :: Draft -> m (),
    hEditCategoryId :: DraftId -> CategoryId -> m (),
    hEditTagId :: DraftId -> [TagId] -> m (),
    hEditName :: DraftId -> Name -> m (),
    hEditDescription :: DraftId -> Description -> m (),
    hEditMainPhoto :: DraftId -> Image -> m (),
    hDoesExist :: DraftId -> m Bool,
    hIsAuthor :: Token -> m Bool,
    hHasPost :: DraftId -> m Bool,
    hDelete :: DraftId -> m (),
    hGetCurrentDate :: m String,
    hGet :: DraftId -> m Draft,
    hGetAuthor :: DraftId -> m AuthorId,
    hPublish :: Draft -> String -> m PostId,
    hUpdate :: Draft -> m PostId,
    hGetAuthorIdByToken :: Token -> m AuthorId,
    hAddMinorPhoto :: DraftId -> Image -> m (),
    hDeleteMinorPhoto :: DraftId -> ImageId -> m ()
}

create :: Monad m => Handle m -> Draft -> m ()
create = hCreate

get :: Monad m => Handle m -> DraftId -> Token -> m (Either Text Draft)
get handle draftId token = do
    hasRights <- hasRights handle draftId token
    case hasRights of
        Left l -> return $ Left l
        Right _ -> Right <$> hGet handle draftId

edit :: Monad m => Handle m -> DraftId -> Token -> EditParams -> m (Either Text ())
edit handle draftId token params = do
    hasRights <- hasRights handle draftId token
    case hasRights of
        Left l -> return $ Left l 
        Right _ -> do
            case eCategoryId params of
                Just x -> hEditCategoryId handle draftId x
                _ -> return ()
            case eTagId params of
                Just x -> hEditTagId handle draftId x
                _ -> return ()
            case eName params of
                Just x -> hEditName handle draftId x
                _ -> return ()
            case eDescription params of
                Just x -> hEditDescription handle draftId x
                _ -> return ()
            case eMainPhoto params of
                Just x -> hEditMainPhoto handle draftId x
                _ -> return ()
            return $ Right ()

delete :: Monad m => Handle m -> DraftId -> Token -> m (Either Text ())
delete handle draftId token = do
                hasRights <- hasRights handle draftId token
                hasPost <- hHasPost handle draftId
                let canDelete = if hasPost then Left "No rights to delete" else Right ()
                case hasRights >> canDelete of
                    Left l -> return $ Left l
                    Right _ -> do
                        hDelete handle draftId
                        return $ Right ()

publish :: Monad m => Handle m -> DraftId -> Token -> m (Either Text PostId)
publish handle draftId token = do
    hasRights <- hasRights handle draftId token
    case hasRights of
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
    hasRights <- hasRights handle draftId token 
    case hasRights of 
        Left l -> return $ Left l
        Right _ -> Right <$> hAddMinorPhoto handle draftId image

deleteMinorPhoto :: Monad m => Handle m -> DraftId -> Token -> ImageId -> m (Either Text ())
deleteMinorPhoto handle draftId token imageId = do 
    hasRights <- hasRights handle draftId token 
    case hasRights of 
        Left l -> return $ Left l
        Right _ -> Right <$> hDeleteMinorPhoto handle draftId imageId

hasRights :: Monad m => Handle m -> DraftId -> Token -> m (Either Text ())
hasRights handle draftId token = do
    exist <- doesExist handle draftId 
    case exist of 
        Left l -> return $ Left l 
        Right _ -> do 
            author <- isAuthor handle draftId token
            case author of 
                Left l -> return $ Left l 
                Right _ -> return $ Right ()

isAuthor :: Monad m => Handle m -> DraftId -> Token -> m (Either Text ())
isAuthor handle draftId token = do
    isAuthor <- hIsAuthor handle token
    if isAuthor
        then do
            authorId <- hGetAuthorIdByToken handle token
            realAuthorId <- hGetAuthor handle draftId
            if authorId == realAuthorId
                then return $ Right ()
                else return $ Left "This author isn't author of the draft"
        else return $ Left "The user isn't author"

doesExist :: Monad m => Handle m -> DraftId -> m (Either Text ())
doesExist handle draftId = do 
    exist <- hDoesExist handle draftId 
    if exist 
        then return $ Right ()
        else return $ Left "Draft with such id doesn't exist"