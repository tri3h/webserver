module Handlers.Draft
  ( Handle (..),
    create,
    get,
    edit,
    delete,
    publish,
    addMinorPhoto,
    deleteMinorPhoto,
    isAuthor,
    getAuthorIdByToken,
  )
where

import Data.Foldable (forM_)
import Data.Text (Text)
import Types.Author (AuthorId)
import Types.Category (CategoryId)
import Types.Config (ServerAddress)
import Types.Draft
  ( CreateDraft (..),
    DraftId,
    EditParams (..),
    GetDraft (..),
    Name,
    noDeleteHasPost,
    noDraftAuthor,
    userNotAuthor,
  )
import Types.Image (Image, ImageId, Link)
import Types.Tag (TagId)
import Types.User (Token)
import Utility (imageIdToLink)
import Prelude hiding (words)

data Handle m = Handle
  { hCreate :: CreateDraft -> m (),
    hEditCategoryId :: DraftId -> CategoryId -> m (),
    hEditTagId :: DraftId -> [TagId] -> m (),
    hEditName :: DraftId -> Name -> m (),
    hEditDescription :: DraftId -> Text -> m (),
    hEditMainPhoto :: DraftId -> Image -> m (),
    hDelete :: DraftId -> m (),
    hAddMinorPhoto :: DraftId -> Image -> m (),
    hDeleteMinorPhoto :: DraftId -> ImageId -> m (),
    hIsAuthor :: Token -> m Bool,
    hHasPost :: DraftId -> m Bool,
    hGet :: DraftId -> (ImageId -> Link) -> m GetDraft,
    hGetAuthor :: DraftId -> m AuthorId,
    hPublish :: DraftId -> m (),
    hUpdate :: DraftId -> m (),
    hGetAuthorIdByToken :: Token -> m AuthorId,
    hDoesExist :: DraftId -> m (Either Text ()),
    hDoesAuthorExist :: AuthorId -> m (Either Text ()),
    hDoesCategoryExist :: CategoryId -> m (Either Text ()),
    hDoesTagExist :: TagId -> m (Either Text ())
  }

create :: Monad m => Handle m -> CreateDraft -> m (Either Text ())
create handle draft = do
  authorExist <- hDoesAuthorExist handle $ cAuthorId draft
  categoryExist <- hDoesCategoryExist handle $ cCategoryId draft
  tagExist <- foldl1 (>>) $ map (hDoesTagExist handle) $ cTagId draft
  case authorExist >> categoryExist >> tagExist of
    Right _ -> Right <$> hCreate handle draft
    Left l -> return $ Left l

get :: Monad m => Handle m -> ServerAddress -> DraftId -> Token -> m (Either Text GetDraft)
get handle server draftId token = do
  access <- canAccess handle draftId token
  case access of
    Left l -> return $ Left l
    Right _ -> Right <$> hGet handle draftId (imageIdToLink server)

edit :: Monad m => Handle m -> DraftId -> Token -> EditParams -> m (Either Text ())
edit handle draftId token params = do
  access <- canAccess handle draftId token
  case access of
    Left l -> return $ Left l
    Right _ -> do
      forM_ (eName params) (hEditName handle draftId)
      forM_ (eText params) (hEditDescription handle draftId)
      resCateg <- case eCategoryId params of
        Nothing -> return $ Right ()
        Just categId -> editCategoryId handle draftId categId
      resTag <- case eTagId params of
        Nothing -> return $ Right ()
        Just tId -> editTagId handle draftId tId
      resPhoto <- editPhoto handle draftId params
      return (resCateg >> resTag >> resPhoto)

editPhoto :: Monad m => Handle m -> DraftId -> EditParams -> m (Either Text ())
editPhoto handle draftId params =
  case eMainPhoto params of
    Just x -> Right <$> hEditMainPhoto handle draftId x
    _ -> return $ Right ()

editCategoryId :: Monad m => Handle m -> DraftId -> CategoryId -> m (Either Text ())
editCategoryId handle draftId categId = do
  exist <- hDoesCategoryExist handle categId
  case exist of
    Left l -> return $ Left l
    Right _ -> Right <$> hEditCategoryId handle draftId categId

editTagId :: Monad m => Handle m -> DraftId -> [TagId] -> m (Either Text ())
editTagId handle draftId tId = do
  exist <- foldl1 (>>) $ map (hDoesTagExist handle) tId
  case exist of
    Left l -> return $ Left l
    Right _ -> Right <$> hEditTagId handle draftId tId

delete :: Monad m => Handle m -> DraftId -> Token -> m (Either Text ())
delete handle draftId token = do
  access <- canAccess handle draftId token
  hasPost <- hHasPost handle draftId
  let canDelete = if hasPost then Left noDeleteHasPost else Right ()
  case access >> canDelete of
    Left l -> return $ Left l
    Right _ -> Right <$> hDelete handle draftId

publish :: Monad m => Handle m -> DraftId -> Token -> m (Either Text ())
publish handle draftId token = do
  access <- canAccess handle draftId token
  case access of
    Left l -> return $ Left l
    Right _ -> do
      hasPost <- hHasPost handle draftId
      if hasPost
        then Right <$> hUpdate handle draftId
        else Right <$> hPublish handle draftId

addMinorPhoto :: Monad m => Handle m -> DraftId -> Token -> Image -> m (Either Text ())
addMinorPhoto handle draftId token image = do
  access <- canAccess handle draftId token
  case access of
    Left l -> return $ Left l
    Right _ -> Right <$> hAddMinorPhoto handle draftId image

deleteMinorPhoto :: Monad m => Handle m -> DraftId -> Token -> ImageId -> m (Either Text ())
deleteMinorPhoto handle draftId token imageId = do
  access <- canAccess handle draftId token
  case access of
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
    Right authId -> do
      realAuthorId <- hGetAuthor handle draftId
      if authId == realAuthorId
        then return $ Right ()
        else return $ Left noDraftAuthor
    Left l -> return $ Left l

getAuthorIdByToken :: Monad m => Handle m -> Token -> m (Either Text AuthorId)
getAuthorIdByToken handle token = do
  author <- hIsAuthor handle token
  if author
    then Right <$> hGetAuthorIdByToken handle token
    else return $ Left userNotAuthor
