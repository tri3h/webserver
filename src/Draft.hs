{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Draft where

import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Database.Connection (manage)
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
import Types.Config (Config (database, server), ServerConfig (sAddress))
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

create :: Logger.Handle IO -> Config -> QueryText -> ByteString -> Token -> IO Response
create logger config query body token = do
  author <- Handler.getAuthorIdByToken (handle config) token
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
      result <- Handler.create (handle config) draft
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

get :: Logger.Handle IO -> Config -> QueryText -> IO Response
get logger config query = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right (draftId, token) -> do
      result <- Handler.get (handle config) (sAddress $ server config) draftId token
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

delete :: Logger.Handle IO -> Config -> QueryText -> IO Response
delete logger config query = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right (draftId, token) -> do
      result <- Handler.delete (handle config) draftId token
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

edit :: Logger.Handle IO -> Config -> QueryText -> ByteString -> IO Response
edit logger config query body = do
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
      result <- Handler.edit (handle config) draftId token editParams
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

publish :: Logger.Handle IO -> Config -> QueryText -> IO Response
publish logger config query = do
  let info = getDraftIdToken query
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right (draftId, token) -> do
      result <- Handler.publish (handle config) draftId token
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

addMinorPhoto :: Logger.Handle IO -> Config -> QueryText -> ByteString -> IO Response
addMinorPhoto logger config query body = do
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
      result <- Handler.addMinorPhoto (handle config) draftId token (Image image imageType)
      Logger.debug logger $ "Tried to add minor photo to draft and got: " ++ show result
      case result of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right _ -> return $ responseLBS status201 [] ""

deleteMinorPhoto :: Logger.Handle IO -> Config -> QueryText -> IO Response
deleteMinorPhoto logger config query = do
  let info = do
        draftId <- getInteger query "draft_id"
        token <- getText query "token"
        imageId <- getInteger query "minor_photo_id"
        Right (draftId, token, imageId)
  Logger.debug logger $ "Tried to parse query and got: " ++ show info
  case info of
    Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
    Right (draftId, token, imageId) -> do
      result <- Handler.deleteMinorPhoto (handle config) draftId token imageId
      Logger.debug logger $ "Tried to delete minor photo to draft and got: " ++ show result
      case result of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right _ -> return $ responseLBS status204 [] ""

getDraftIdToken :: QueryText -> Either Text (Integer, Text)
getDraftIdToken query = do
  draftId <- getInteger query "draft_id"
  token <- getText query "token"
  Right (draftId, token)

handle :: Config -> Handler.Handle IO
handle config =
  let db = database config
   in Handler.Handle
        { Handler.hCreate = manage db . Db.Draft.create,
          Handler.hEditCategoryId = \a b -> manage db $ Db.Draft.editCategoryId a b,
          Handler.hEditTagId = \a b -> manage db $ Db.Draft.editTagId a b,
          Handler.hEditName = \a b -> manage db $ Db.Draft.editName a b,
          Handler.hEditDescription = \a b -> manage db $ Db.Draft.editDescription a b,
          Handler.hEditMainPhoto = \a b -> manage db $ Db.Draft.editMainPhoto a b,
          Handler.hDoesExist = manage db . Db.Draft.doesExist,
          Handler.hIsAuthor = manage db . Db.Author.isAuthor,
          Handler.hHasPost = manage db . Db.Draft.hasPost,
          Handler.hDelete = manage db . Db.Draft.delete,
          Handler.hGetCurrentDate = show <$> getCurrentTime,
          Handler.hGet = manage db . Db.Draft.get,
          Handler.hGetAuthor = manage db . Db.Author.getByDraftId,
          Handler.hPublish = \a b -> manage db $ Db.Draft.publish a b,
          Handler.hUpdate = manage db . Db.Draft.update,
          Handler.hGetAuthorIdByToken = manage db . Db.Author.getByToken,
          Handler.hAddMinorPhoto = \a b -> manage db $ Db.Draft.addMinorPhoto a b,
          Handler.hDeleteMinorPhoto = \a b -> manage db $ Db.Draft.deleteMinorPhoto a b,
          Handler.hDoesAuthorExist = manage db . Db.Author.doesExist,
          Handler.hDoesCategoryExist = manage db . Db.Category.doesExist,
          Handler.hDoesTagExist = manage db . Db.Tag.doesExist
        }
