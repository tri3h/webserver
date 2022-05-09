module Handlers.Draft
  ( Handle (..),
    get,
    edit,
    delete,
    publish,
    addMinorPhoto,
    deleteMinorPhoto,
  )
where

import Data.Text (Text)
import Error (Error, noDeleteHasPost, noDraftAuthor)
import Types.Author (AuthorId)
import Types.Category (CategoryId)
import Types.Config (ServerAddress)
import Types.Draft
  ( CreateDraft (..),
    DraftId,
    EditParams (..),
    GetDraft (..),
    Name,
  )
import Types.Image (Image, ImageId, Link)
import Types.Tag (TagId)
import Types.User (Token)
import Utility (imageIdToLink)
import Prelude hiding (words)

data Handle m = Handle
  { hCreate :: CreateDraft -> m (Either Error ()),
    hEditCategoryId :: DraftId -> CategoryId -> m (Either Error ()),
    hEditTagId :: DraftId -> [TagId] -> m (Either Error ()),
    hEditName :: DraftId -> Name -> m (Either Error ()),
    hEditDescription :: DraftId -> Text -> m (Either Error ()),
    hEditMainPhoto :: DraftId -> Image -> m (Either Error ()),
    hDelete :: DraftId -> m (Either Error ()),
    hAddMinorPhoto :: DraftId -> Image -> m (Either Error ()),
    hDeleteMinorPhoto :: DraftId -> ImageId -> m (Either Error ()),
    hHasPost :: DraftId -> m Bool,
    hGet :: DraftId -> (ImageId -> Link) -> m GetDraft,
    hGetAuthorIdByDraftId :: DraftId -> m (Either Error AuthorId),
    hPublish :: DraftId -> m (Either Error ()),
    hUpdate :: DraftId -> m (Either Error ()),
    hGetAuthorIdByToken :: Token -> m (Either Error AuthorId)
  }

get :: Monad m => Handle m -> ServerAddress -> DraftId -> Token -> m (Either Error GetDraft)
get handle server draftId token = do
  access <- canAccess handle draftId token
  case access of
    Left l -> return $ Left l
    Right _ -> Right <$> hGet handle draftId (imageIdToLink server)

edit :: Monad m => Handle m -> DraftId -> Token -> EditParams -> m (Either Error ())
edit handle draftId token params = do
  access <- canAccess handle draftId token
  case access of
    Left l -> return $ Left l
    Right _ -> do
      resName <- case eName params of
        Nothing -> return $ Right ()
        Just name -> hEditName handle draftId name
      resDescr <- case eText params of
        Nothing -> return $ Right ()
        Just t -> hEditDescription handle draftId t
      resCateg <- case eCategoryId params of
        Nothing -> return $ Right ()
        Just categId -> hEditCategoryId handle draftId categId
      resTag <- case eTagId params of
        Nothing -> return $ Right ()
        Just tId -> hEditTagId handle draftId tId
      resPhoto <- editPhoto handle draftId params
      return (resName >> resDescr >> resCateg >> resTag >> resPhoto)

editPhoto :: Monad m => Handle m -> DraftId -> EditParams -> m (Either Error ())
editPhoto handle draftId params =
  case eMainPhoto params of
    Just x -> hEditMainPhoto handle draftId x
    _ -> return $ Right ()

delete :: Monad m => Handle m -> DraftId -> Token -> m (Either Error ())
delete handle draftId token = do
  access <- canAccess handle draftId token
  hasPost <- hHasPost handle draftId
  let canDelete = if hasPost then Left noDeleteHasPost else Right ()
  case access >> canDelete of
    Left l -> return $ Left l
    Right _ -> hDelete handle draftId

publish :: Monad m => Handle m -> DraftId -> Token -> m (Either Error ())
publish handle draftId token = do
  access <- canAccess handle draftId token
  case access of
    Left l -> return $ Left l
    Right _ -> do
      hasPost <- hHasPost handle draftId
      if hasPost
        then hUpdate handle draftId
        else hPublish handle draftId

addMinorPhoto :: Monad m => Handle m -> DraftId -> Token -> Image -> m (Either Error ())
addMinorPhoto handle draftId token image = do
  access <- canAccess handle draftId token
  case access of
    Left l -> return $ Left l
    Right _ -> hAddMinorPhoto handle draftId image

deleteMinorPhoto :: Monad m => Handle m -> DraftId -> Token -> ImageId -> m (Either Error ())
deleteMinorPhoto handle draftId token imageId = do
  access <- canAccess handle draftId token
  case access of
    Left l -> return $ Left l
    Right _ -> hDeleteMinorPhoto handle draftId imageId

canAccess :: Monad m => Handle m -> DraftId -> Token -> m (Either Error ())
canAccess handle draftId token = do
  author <- hGetAuthorIdByToken handle token
  realAuthor <- hGetAuthorIdByDraftId handle draftId
  if author == realAuthor
    then return $ Right ()
    else return $ author >> realAuthor >> Left noDraftAuthor
