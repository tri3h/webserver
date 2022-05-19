{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Draft where

import Data.ByteString (ByteString)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Author as Db.Author
import qualified Database.Queries.Draft as Db.Draft
import Error (Error)
import qualified Handlers.Draft as Handler
import qualified Handlers.Logger as Logger
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response)
import Types.Category (CategoryId (CategoryId))
import Types.Config (ServerAddress)
import Types.Draft
  ( CreateDraft (..),
    DraftId (DraftId),
    EditParams (..),
    Name (Name),
    draftsOnPage,
  )
import Types.Image (Image (..), ImageId (ImageId))
import Types.Limit (Offset (Offset))
import Types.Tag (TagId (TagId))
import Types.User (Token (Token))
import Utility
  ( getImage,
    getInteger,
    getLimit,
    getMaybeImage,
    getMaybeInteger,
    getMaybeIntegers,
    getMaybeText,
    getOffset,
    getText,
    response200JSON,
    response201,
    response204,
    response400,
  )
import Prelude hiding (words)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> ByteString -> Token -> IO Response
create logger pool query body cToken = do
  let cTagId = getMaybeTagId query
  let cMainPhoto = getMaybeMainPhoto body
  let info = do
        cCategoryId <- getCategoryId query
        cName <- getName query
        cText <- getText query "description"
        Right $
          CreateDraft {..}
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right draft -> do
      result <- Handler.hCreate (handle pool) draft
      Logger.debug logger $ "Tried to create draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right r -> response200JSON r

get :: Logger.Handle IO -> Pool Connection -> ServerAddress -> QueryText -> IO Response
get logger pool server query = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right (draftId, token) -> do
      result <- Handler.get (handle pool) server draftId token
      Logger.debug logger $ "Tried to get draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right draft -> response200JSON draft

getAllByAuthor :: Logger.Handle IO -> Pool Connection -> Token -> QueryText -> IO Response
getAllByAuthor logger pool token query = do
  let limit = getLimit query draftsOnPage
  let offset = getOffset query (Offset 0)
  result <- Handler.hGetAllByAuthor (handle pool) token limit offset
  Logger.debug logger $ "Tried to get a list of draft id and got: " ++ show result
  return $ response200JSON result

delete :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
delete logger pool query = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right (draftId, token) -> do
      result <- Handler.delete (handle pool) draftId token
      Logger.debug logger $ "Tried to delete draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response204

edit :: Logger.Handle IO -> Pool Connection -> QueryText -> ByteString -> IO Response
edit logger pool query body = do
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
      result <- Handler.edit (handle pool) draftId token editParams
      Logger.debug logger $ "Tried to edit draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response201

publish :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
publish logger pool query = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right (draftId, token) -> do
      result <- Handler.publish (handle pool) draftId token
      Logger.debug logger $ "Tried to publish draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response201

addMinorPhoto :: Logger.Handle IO -> Pool Connection -> QueryText -> ByteString -> IO Response
addMinorPhoto logger pool query body = do
  let info = do
        draftId <- getDraftId query
        token <- getToken query
        image <- getMinorPhoto body
        Right (draftId, token, image)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right (draftId, token, image) -> do
      result <- Handler.addMinorPhoto (handle pool) draftId token image
      Logger.debug logger $ "Tried to add minor photo to draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response201

deleteMinorPhoto :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
deleteMinorPhoto logger pool query = do
  let info = do
        draftId <- getDraftId query
        token <- getToken query
        imageId <- getMinorPhotoId query
        Right (draftId, token, imageId)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ response400 l
    Right (draftId, token, imageId) -> do
      result <- Handler.deleteMinorPhoto (handle pool) draftId token imageId
      Logger.debug logger $ "Tried to delete minor photo to draft and got: " ++ show result
      return $ case result of
        Left l -> response400 l
        Right _ -> response204

getDraftIdToken :: QueryText -> Either Error (DraftId, Token)
getDraftIdToken query = do
  draftId <- getInteger query "draft_id"
  token <- getText query "token"
  Right (DraftId draftId, Token token)

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

handle :: Pool Connection -> Handler.Handle IO
handle pool =
  let f = withResource pool
   in Handler.Handle
        { Handler.hCreate = f . Db.Draft.create,
          Handler.hEditCategoryId = \a b -> f $ Db.Draft.editCategoryId a b,
          Handler.hEditTagId = \a b -> f $ Db.Draft.editTagId a b,
          Handler.hEditName = \a b -> f $ Db.Draft.editName a b,
          Handler.hEditDescription = \a b -> f $ Db.Draft.editText a b,
          Handler.hEditMainPhoto = \a b -> f $ Db.Draft.editMainPhoto a b,
          Handler.hHasPost = f . Db.Draft.hasPost,
          Handler.hDelete = f . Db.Draft.delete,
          Handler.hGet = \a b -> f $ Db.Draft.get a b,
          Handler.hGetAllByAuthor = \a b c -> f $ Db.Draft.getAllByAuthor a b c,
          Handler.hGetAuthorIdByDraftId = f . Db.Author.getByDraftId,
          Handler.hPublish = f . Db.Draft.publish,
          Handler.hUpdate = f . Db.Draft.update,
          Handler.hGetAuthorIdByToken = f . Db.Author.getByToken,
          Handler.hAddMinorPhoto = \a b -> f $ Db.Draft.addMinorPhoto a b,
          Handler.hDeleteMinorPhoto = \a b -> f $ Db.Draft.deleteMinorPhoto a b
        }
