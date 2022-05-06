{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Draft where

import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Author as Db.Author
import qualified Database.Queries.Category as Db.Category
import qualified Database.Queries.Draft as Db.Draft
import qualified Database.Queries.Tag as Db.Tag
import qualified Handlers.Draft as Handler
import qualified Handlers.Logger as Logger
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status
  ( status200,
    status201,
    status204,
    status400,
  )
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response, responseLBS)
import Types.Category (CategoryId (CategoryId))
import Types.Config (ServerAddress)
import Types.Draft
  ( CreateDraft (..),
    DraftId (DraftId),
    EditParams (..),
    Name (Name),
  )
import Types.Image (Image (..), ImageId (ImageId))
import Types.Tag (TagId (TagId))
import Types.User (Token (Token))
import Utility
  ( getImage,
    getInteger,
    getIntegers,
    getMaybeImage,
    getMaybeInteger,
    getMaybeIntegers,
    getMaybeText,
    getText,
  )
import Prelude hiding (words)

create :: Logger.Handle IO -> Pool Connection -> QueryText -> ByteString -> Token -> IO Response
create logger pool query body token = do
  author <- Handler.getAuthorIdByToken (handle pool) token
  let info = do
        cCategoryId <- getCategoryId query
        cTagId <- getTagIds query
        cName <- getName query
        cText <- getText query "description"
        cMainPhoto <- getMainPhoto body
        cAuthorId <- author
        Right $
          CreateDraft {..}
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l ->
      return $
        responseLBS
          status400
          []
          . encodeUtf8
          $ LazyText.fromStrict l
    Right draft -> do
      result <- Handler.create (handle pool) draft
      Logger.debug logger $ "Tried to create draft and got: " ++ show result
      case result of
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
        Right _ -> return $ responseLBS status201 [] ""

get :: Logger.Handle IO -> Pool Connection -> ServerAddress -> QueryText -> IO Response
get logger pool server query = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right (draftId, token) -> do
      result <- Handler.get (handle pool) server draftId token
      Logger.debug logger $ "Tried to get draft and got: " ++ show result
      case result of
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
        Right draft ->
          return $
            responseLBS
              status200
              [(hContentType, "application/json")]
              $ encode draft

delete :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
delete logger pool query = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right (draftId, token) -> do
      result <- Handler.delete (handle pool) draftId token
      Logger.debug logger $ "Tried to delete draft and got: " ++ show result
      case result of
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
        Right _ -> return $ responseLBS status204 [] ""

edit :: Logger.Handle IO -> Pool Connection -> QueryText -> ByteString -> IO Response
edit logger pool query body = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
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
      case result of
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
        Right _ -> return $ responseLBS status201 [] ""

publish :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
publish logger pool query = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right (draftId, token) -> do
      result <- Handler.publish (handle pool) draftId token
      Logger.debug logger $ "Tried to publish draft and got: " ++ show result
      case result of
        Left l ->
          return $
            responseLBS
              status400
              []
              . encodeUtf8
              $ LazyText.fromStrict l
        Right _ ->
          return $
            responseLBS
              status201
              []
              ""

addMinorPhoto :: Logger.Handle IO -> Pool Connection -> QueryText -> ByteString -> IO Response
addMinorPhoto logger pool query body = do
  let info = do
        draftId <- getDraftId query
        token <- getToken query
        image <- getMinorPhoto body
        Right (draftId, token, image)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right (draftId, token, image) -> do
      result <- Handler.addMinorPhoto (handle pool) draftId token image
      Logger.debug logger $ "Tried to add minor photo to draft and got: " ++ show result
      case result of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right _ -> return $ responseLBS status201 [] ""

deleteMinorPhoto :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
deleteMinorPhoto logger pool query = do
  let info = do
        draftId <- getDraftId query
        token <- getToken query
        imageId <- getMinorPhotoId query
        Right (draftId, token, imageId)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right (draftId, token, imageId) -> do
      result <- Handler.deleteMinorPhoto (handle pool) draftId token imageId
      Logger.debug logger $ "Tried to delete minor photo to draft and got: " ++ show result
      case result of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right _ -> return $ responseLBS status204 [] ""

getDraftIdToken :: QueryText -> Either Text (DraftId, Token)
getDraftIdToken query = do
  draftId <- getInteger query "draft_id"
  token <- getText query "token"
  Right (DraftId draftId, Token token)

getCategoryId :: QueryText -> Either Text CategoryId
getCategoryId query = CategoryId <$> getInteger query "category_id"

getTagIds :: QueryText -> Either Text [TagId]
getTagIds query = map TagId <$> getIntegers query "tag_id"

getName :: QueryText -> Either Text Name
getName query = Name <$> getText query "name"

getMainPhoto :: ByteString -> Either Text Image
getMainPhoto body = getImage body "main_photo"

getMinorPhoto :: ByteString -> Either Text Image
getMinorPhoto body = getImage body "minor_photo"

getMinorPhotoId :: QueryText -> Either Text ImageId
getMinorPhotoId query = ImageId <$> getInteger query "minor_photo_id"

getDraftId :: QueryText -> Either Text DraftId
getDraftId query = DraftId <$> getInteger query "draft_id"

getToken :: QueryText -> Either Text Token
getToken query = Token <$> getText query "token"

getMaybeCategoryId :: QueryText -> Maybe CategoryId
getMaybeCategoryId query = CategoryId <$> getMaybeInteger query "category_id"

getMaybeTagId :: QueryText -> Maybe [TagId]
getMaybeTagId query = map TagId <$> getMaybeIntegers query "tag_id"

getMaybeName :: QueryText -> Maybe Name
getMaybeName query = Name <$> getMaybeText query "name"

getMaybeMainPhoto :: ByteString -> Maybe Image
getMaybeMainPhoto body = getMaybeImage body "main_photo"

handle :: Pool Connection -> Handler.Handle IO
handle pool =
  Handler.Handle
    { Handler.hCreate = withResource pool . Db.Draft.create,
      Handler.hEditCategoryId = \a b -> withResource pool $ Db.Draft.editCategoryId a b,
      Handler.hEditTagId = \a b -> withResource pool $ Db.Draft.editTagId a b,
      Handler.hEditName = \a b -> withResource pool $ Db.Draft.editName a b,
      Handler.hEditDescription = \a b -> withResource pool $ Db.Draft.editText a b,
      Handler.hEditMainPhoto = \a b -> withResource pool $ Db.Draft.editMainPhoto a b,
      Handler.hDoesExist = withResource pool . Db.Draft.doesExist,
      Handler.hIsAuthor = withResource pool . Db.Author.isAuthor,
      Handler.hHasPost = withResource pool . Db.Draft.hasPost,
      Handler.hDelete = withResource pool . Db.Draft.delete,
      Handler.hGet = \a f -> withResource pool $ Db.Draft.get a f,
      Handler.hGetAuthor = withResource pool . Db.Author.getByDraftId,
      Handler.hPublish = withResource pool . Db.Draft.publish,
      Handler.hUpdate = withResource pool . Db.Draft.update,
      Handler.hGetAuthorIdByToken = withResource pool . Db.Author.getByToken,
      Handler.hAddMinorPhoto = \a b -> withResource pool $ Db.Draft.addMinorPhoto a b,
      Handler.hDeleteMinorPhoto = \a b -> withResource pool $ Db.Draft.deleteMinorPhoto a b,
      Handler.hDoesAuthorExist = withResource pool . Db.Author.doesExist,
      Handler.hDoesCategoryExist = withResource pool . Db.Category.doesExist,
      Handler.hDoesTagExist = withResource pool . Db.Tag.doesExist
    }
