{-# LANGUAGE OverloadedStrings #-}

module Handlers.Draft
  ( Handle (..),
    get,
    edit,
    delete,
    publish,
    addMinorPhoto,
    deleteMinorPhoto,
    create,
    getAllByAuthor,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Error (Error, noDeleteHasPost, noDraftAuthor)
import qualified Handlers.Logger as Logger
import Network.HTTP.Types (QueryText)
import Network.Wai (Response)
import Types.Author (AuthorId)
import Types.Category (CategoryId (CategoryId))
import Types.Config (ServerAddress)
import Types.Draft
  ( CreateDraft (CreateDraft, cCategoryId, cMainPhoto, cName, cTagId, cText, cToken),
    DraftId (DraftId),
    EditParams (..),
    GetDraft (..),
    Name (Name),
    draftsOnPage,
  )
import Types.Image (Image, ImageId (ImageId), Link)
import Types.Limit (Limit, Offset (Offset))
import Types.Tag (TagId (TagId))
import Types.User (Token (Token))
import Utility (getImage, getInteger, getLimit, getMaybeImage, getMaybeInteger, getMaybeIntegers, getMaybeText, getOffset, getText, imageIdToLink, response200JSON, response201, response204, response400)
import Prelude hiding (words)

data Handle m = Handle
  { hEditCategoryId :: DraftId -> CategoryId -> m (Either Error ()),
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
    hGetAuthorIdByToken :: Token -> m (Either Error AuthorId),
    hCreate :: CreateDraft -> m (Either Error DraftId),
    hGetAllByAuthor :: Token -> Limit -> Offset -> m [DraftId]
  }

create :: Monad m => Handle m -> Logger.Handle m -> QueryText -> ByteString -> Token -> m Response
create handle logger query body token = do
  let tagId = getMaybeTagId query
  let mainPhoto = getMaybeMainPhoto body
  let info = do
        categoryId <- getCategoryId query
        name <- getName query
        text <- getText query "description"
        Right $
          CreateDraft
            { cToken = token,
              cTagId = tagId,
              cMainPhoto = mainPhoto,
              cCategoryId = categoryId,
              cName = name,
              cText = text
            }
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right draft -> do
      result <- hCreate handle draft
      Logger.debug logger $ "Tried to create draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right r -> response200JSON r

get :: Monad m => Handle m -> Logger.Handle m -> ServerAddress -> QueryText -> m Response
get handle logger server query = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right (draftId, token) -> do
      result <- getDraft handle server draftId token
      Logger.debug logger $ "Tried to get draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right draft -> response200JSON draft

getAllByAuthor :: Monad m => Handle m -> Logger.Handle m -> Token -> QueryText -> m Response
getAllByAuthor handle logger token query = do
  let limit = getLimit query draftsOnPage
  let offset = getOffset query (Offset 0)
  result <- hGetAllByAuthor handle token limit offset
  Logger.debug logger $ "Tried to get a list of draft id and got: " ++ show result
  return $ response200JSON result

delete :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
delete handle logger query = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right (draftId, token) -> do
      result <- deleteDraft handle draftId token
      Logger.debug logger $ "Tried to delete draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response204

edit :: Monad m => Handle m -> Logger.Handle m -> QueryText -> ByteString -> m Response
edit handle logger query body = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right (draftId, token) -> do
      let editParams =
            EditParams
              { eCategoryId = getMaybeCategoryId query,
                eTagId = getMaybeTagId query,
                eName = getMaybeName query,
                eText = getMaybeText query "description",
                eMainPhoto = getMaybeMainPhoto body
              }
      result <- editDraft handle draftId token editParams
      Logger.debug logger $ "Tried to edit draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response201

publish :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
publish handle logger query = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right (draftId, token) -> do
      result <- publishDraft handle draftId token
      Logger.debug logger $ "Tried to publish draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response201

addMinorPhoto :: Monad m => Handle m -> Logger.Handle m -> QueryText -> ByteString -> m Response
addMinorPhoto handle logger query body = do
  let info = do
        draftId <- getDraftId query
        token <- getToken query
        image <- getMinorPhoto body
        Right (draftId, token, image)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right (draftId, token, image) -> do
      result <- add handle draftId token image
      Logger.debug logger $ "Tried to add minor photo to draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response201
  where
    add :: Monad m => Handle m -> DraftId -> Token -> Image -> m (Either Error ())
    add h draftId token image = do
      access <- canAccess h draftId token
      case access of
        Left l -> return $ Left l
        Right _ -> hAddMinorPhoto h draftId image

deleteMinorPhoto :: Monad m => Handle m -> Logger.Handle m -> QueryText -> m Response
deleteMinorPhoto handle logger query = do
  let info = do
        draftId <- getDraftId query
        token <- getToken query
        imageId <- getMinorPhotoId query
        Right (draftId, token, imageId)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right (draftId, token, imageId) -> do
      result <- deletePhoto handle draftId token imageId
      Logger.debug logger $ "Tried to delete minor photo to draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response204
  where
    deletePhoto :: Monad m => Handle m -> DraftId -> Token -> ImageId -> m (Either Error ())
    deletePhoto h draftId token imageId = do
      access <- canAccess h draftId token
      case access of
        Left l -> return $ Left l
        Right _ -> hDeleteMinorPhoto h draftId imageId

getDraftIdToken :: QueryText -> Either Error (DraftId, Token)
getDraftIdToken query = do
  draftId <- getInteger query "draft_id"
  token <- getText query "token"
  Right (DraftId draftId, Token token)

getDraft :: Monad m => Handle m -> ServerAddress -> DraftId -> Token -> m (Either Error GetDraft)
getDraft handle server draftId token = do
  access <- canAccess handle draftId token
  case access of
    Left l -> return $ Left l
    Right _ -> Right <$> hGet handle draftId (imageIdToLink server)

editDraft :: Monad m => Handle m -> DraftId -> Token -> EditParams -> m (Either Error ())
editDraft handle draftId token params = do
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
        [] -> return $ Right ()
        x -> hEditTagId handle draftId x
      resPhoto <- editPhoto handle draftId params
      return (resName >> resDescr >> resCateg >> resTag >> resPhoto)

editPhoto :: Monad m => Handle m -> DraftId -> EditParams -> m (Either Error ())
editPhoto handle draftId params =
  case eMainPhoto params of
    Just x -> hEditMainPhoto handle draftId x
    _ -> return $ Right ()

deleteDraft :: Monad m => Handle m -> DraftId -> Token -> m (Either Error ())
deleteDraft handle draftId token = do
  access <- canAccess handle draftId token
  hasPost <- hHasPost handle draftId
  let canDelete = if hasPost then Left noDeleteHasPost else Right ()
  case access >> canDelete of
    Left l -> return $ Left l
    Right _ -> hDelete handle draftId

publishDraft :: Monad m => Handle m -> DraftId -> Token -> m (Either Error ())
publishDraft handle draftId token = do
  access <- canAccess handle draftId token
  case access of
    Left l -> return $ Left l
    Right _ -> do
      hasPost <- hHasPost handle draftId
      if hasPost
        then hUpdate handle draftId
        else hPublish handle draftId

canAccess :: Monad m => Handle m -> DraftId -> Token -> m (Either Error ())
canAccess handle draftId token = do
  author <- hGetAuthorIdByToken handle token
  realAuthor <- hGetAuthorIdByDraftId handle draftId
  if author == realAuthor
    then return $ Right ()
    else return $ author >> realAuthor >> Left noDraftAuthor

getCategoryId :: QueryText -> Either Error CategoryId
getCategoryId query = CategoryId <$> getInteger query "category_id"

getName :: QueryText -> Either Error Name
getName query = Name <$> getText query "name"

getMinorPhoto :: ByteString -> Either Error Image
getMinorPhoto body = getImage body "minor_photo"

getMinorPhotoId :: QueryText -> Either Error ImageId
getMinorPhotoId query = ImageId <$> getInteger query "minor_photo_id"

getDraftId :: QueryText -> Either Error DraftId
getDraftId query = DraftId <$> getInteger query "draft_id"

getToken :: QueryText -> Either Error Token
getToken query = Token <$> getText query "token"

getMaybeCategoryId :: QueryText -> Maybe CategoryId
getMaybeCategoryId query = CategoryId <$> getMaybeInteger query "category_id"

getMaybeTagId :: QueryText -> [TagId]
getMaybeTagId query = map TagId $ getMaybeIntegers query "tag_id"

getMaybeName :: QueryText -> Maybe Name
getMaybeName query = Name <$> getMaybeText query "name"

getMaybeMainPhoto :: ByteString -> Maybe Image
getMaybeMainPhoto body = getMaybeImage body "main_photo"
