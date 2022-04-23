{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Draft where

import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
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
import Types.Config (ServerAddress)
import Types.Draft
  ( Draft
      ( Draft,
        authorId,
        categoryId,
        description,
        draftId,
        mainPhoto,
        minorPhoto,
        name,
        postId,
        tagId
      ),
    EditParams
      ( EditParams,
        eCategoryId,
        eDescription,
        eMainPhoto,
        eName,
        eTagId
      ),
  )
import Types.Image (Image (Image))
import Types.User (Token)
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
        categoryId <- getInteger query "category_id"
        tagId <- getIntegers query "tag_id"
        name <- getText query "name"
        description <- getText query "description"
        imageType <- getText query "image_type"
        image <- getImage (decodeUtf8 body) "main_photo"
        authorId <- author
        Right $
          Draft
            { draftId = Nothing,
              postId = Nothing,
              mainPhoto = Image image imageType,
              minorPhoto = [],
              ..
            }
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
  let info = do
        token <- getText query "token"
        draftId <- getInteger query "draft_id"
        Right (token, draftId)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right (token, draftId) -> do
      let editParams =
            EditParams
              { eCategoryId = getMaybeInteger query "category_id",
                eTagId = getMaybeIntegers query "tag_id",
                eName = getMaybeText query "name",
                eDescription = getMaybeText query "description",
                eMainPhoto = getMainPhoto query body
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

getMainPhoto :: QueryText -> ByteString -> Maybe Image
getMainPhoto query body =
  let imageType = getMaybeText query "image_type"
      image = getMaybeImage (decodeUtf8 body) "main_photo"
   in case imageType of
        Nothing -> Nothing
        Just t -> case image of
          Nothing -> Nothing
          Just i -> Just $ Image i t

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
        draftId <- getInteger query "draft_id"
        token <- getText query "token"
        imageType <- getText query "image_type"
        image <- getImage (decodeUtf8 body) "minor_photo"
        Right (draftId, token, image, imageType)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right (draftId, token, image, imageType) -> do
      result <- Handler.addMinorPhoto (handle pool) draftId token (Image image imageType)
      Logger.debug logger $ "Tried to add minor photo to draft and got: " ++ show result
      case result of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right _ -> return $ responseLBS status201 [] ""

deleteMinorPhoto :: Logger.Handle IO -> Pool Connection -> QueryText -> IO Response
deleteMinorPhoto logger pool query = do
  let info = do
        draftId <- getInteger query "draft_id"
        token <- getText query "token"
        imageId <- getInteger query "minor_photo_id"
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

getDraftIdToken :: QueryText -> Either Text (Integer, Text)
getDraftIdToken query = do
  draftId <- getInteger query "draft_id"
  token <- getText query "token"
  Right (draftId, token)

handle :: Pool Connection -> Handler.Handle IO
handle pool =
  Handler.Handle
    { Handler.hCreate = withResource pool . Db.Draft.create,
      Handler.hEditCategoryId = \a b -> withResource pool $ Db.Draft.editCategoryId a b,
      Handler.hEditTagId = \a b -> withResource pool $ Db.Draft.editTagId a b,
      Handler.hEditName = \a b -> withResource pool $ Db.Draft.editName a b,
      Handler.hEditDescription = \a b -> withResource pool $ Db.Draft.editDescription a b,
      Handler.hEditMainPhoto = \a b -> withResource pool $ Db.Draft.editMainPhoto a b,
      Handler.hDoesExist = withResource pool . Db.Draft.doesExist,
      Handler.hIsAuthor = withResource pool . Db.Author.isAuthor,
      Handler.hHasPost = withResource pool . Db.Draft.hasPost,
      Handler.hDelete = withResource pool . Db.Draft.delete,
      Handler.hGetCurrentDate = show <$> getCurrentTime,
      Handler.hGet = withResource pool . Db.Draft.get,
      Handler.hGetAuthor = withResource pool . Db.Author.getByDraftId,
      Handler.hPublish = \a b -> withResource pool $ Db.Draft.publish a b,
      Handler.hUpdate = withResource pool . Db.Draft.update,
      Handler.hGetAuthorIdByToken = withResource pool . Db.Author.getByToken,
      Handler.hAddMinorPhoto = \a b -> withResource pool $ Db.Draft.addMinorPhoto a b,
      Handler.hDeleteMinorPhoto = \a b -> withResource pool $ Db.Draft.deleteMinorPhoto a b,
      Handler.hDoesAuthorExist = withResource pool . Db.Author.doesExist,
      Handler.hDoesCategoryExist = withResource pool . Db.Category.doesExist,
      Handler.hDoesTagExist = withResource pool . Db.Tag.doesExist
    }
