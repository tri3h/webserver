{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Draft where

import Prelude hiding (words)
import Data.Text (Text, unpack, words)
import Data.Aeson ( encode )
import Types.Draft
    ( Draft(Draft, draftId, postId, authorId, categoryId, tagId, name,
            description, mainPhoto, minorPhoto),
      EditParams(EditParams, eCategoryId, eTagId, eName, eDescription,
                 eMainPhoto) )
import Types.Author(AuthorId)
import Types.Category(CategoryId)
import Types.Tag(TagId)
import Types.Post(PostId, Date)
import Types.User(Token)
import Types.Image ( Image(Image) )
import qualified Handlers.Draft as Handler
import qualified Database.Queries.Draft as Db.Draft
import qualified Database.Queries.Author as Db.Author
import qualified Database.Queries.Category as Db.Category 
import qualified Database.Queries.Tag as Db.Tag
import Database.Connection ( manage )
import Data.Time.Clock ( getCurrentTime )
import Network.HTTP.Types.URI ( QueryText )
import Network.Wai ( Response, responseLBS )
import Network.HTTP.Types.Status
    ( status200, status201, status204, status400 )
import Network.HTTP.Types.Header ( hContentType )
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text.Encoding ( decodeUtf8, decodeUtf16BE )
import qualified Data.Text.Lazy as LazyText
import Utility
    ( getText,
      getInteger,
      getIntegers,
      getImage,
      getMaybeText,
      getMaybeInteger,
      getMaybeIntegers,
      getMaybeImage )
import Data.ByteString ( ByteString )
import qualified Handlers.Logger as Logger

create :: Logger.Handle IO -> QueryText -> ByteString -> Token -> IO Response
create logger query body token = do 
    author <- Handler.getAuthorIdByToken handle token
    let info = do 
            categoryId <- getInteger query "category_id" 
            tagId <- getIntegers query "tag_id"
            name <- getText query "name"
            description <- getText query "description"
            imageType <- getText query "image_type"
            image <- getImage (decodeUtf8 body) "main_photo"
            authorId <- author
            Right $ Draft {
                draftId = Nothing,
                postId = Nothing,
                mainPhoto = Image image imageType,
                minorPhoto = [],
                .. }
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of
        Left l -> return $ responseLBS status400 
            [] . encodeUtf8 $ LazyText.fromStrict l
        Right draft -> do 
            result <- Handler.create handle draft
            Logger.debug logger $ "Tried to create draft and got: " ++ show result
            case result of 
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
                Right _ -> return $ responseLBS status201 [] ""
            

get :: Logger.Handle IO -> QueryText -> IO Response
get logger query = do 
    let info = do 
            draftId <- getInteger query "draft_id"
            token <- getText query "token"
            Right (draftId, token)
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of 
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right (draftId, token) -> do
            result <- Handler.get handle draftId token
            Logger.debug logger $ "Tried to get draft and got: " ++ show result
            case result of 
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
                Right draft -> return $ responseLBS status200 
                    [(hContentType, "application/json")] $ encode draft

delete :: Logger.Handle IO -> QueryText -> IO Response
delete logger query = do 
    let info = do 
            draftId <- getInteger query "draft_id"
            token <- getText query "token"
            Right (draftId, token)
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right (draftId, token) -> do 
            result <- Handler.delete handle draftId token
            Logger.debug logger $ "Tried to delete draft and got: " ++ show result
            case result of
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
                Right _ -> return $ responseLBS status204 [] ""
            
edit :: Logger.Handle IO -> QueryText -> ByteString -> IO Response
edit logger query body = do 
    let info = do 
            token <- getText query "token"
            draftId <- getInteger query "draft_id"
            Right (token, draftId)
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of 
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right (token, draftId) -> do 
            let editParams = EditParams {
                    eCategoryId = getMaybeInteger query "category_id" ,
                    eTagId = getMaybeIntegers query "tag_id",
                    eName = getMaybeText query "name",
                    eDescription = getMaybeText query "description",
                    eMainPhoto = getMainPhoto query body
                    }
            result <- Handler.edit handle draftId token editParams
            Logger.debug logger $ "Tried to edit draft and got: " ++ show result
            case result of 
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
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

publish :: Logger.Handle IO -> QueryText -> IO Response
publish logger query = do 
    let info = do 
            draftId <- getInteger query "draft_id"
            token <- getText query "token"
            Right (draftId, token)
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right (draftId, token) -> do 
            result <- Handler.publish handle draftId token
            Logger.debug logger $ "Tried to publish draft and got: " ++ show result
            case result of
                Left l -> return $ responseLBS status400 
                    [] . encodeUtf8 $ LazyText.fromStrict l
                Right postId -> return $ responseLBS status201 
                    [] ""

addMinorPhoto :: Logger.Handle IO -> QueryText -> ByteString -> IO Response
addMinorPhoto logger query body = do 
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
            result <- Handler.addMinorPhoto handle draftId token (Image image imageType)
            Logger.debug logger $ "Tried to add minor photo to draft and got: " ++ show result
            case result of 
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right _ -> return $ responseLBS status201 [] ""

deleteMinorPhoto :: Logger.Handle IO -> QueryText -> IO Response
deleteMinorPhoto logger query = do 
    let info = do 
            draftId <- getInteger query "draft_id"
            token <- getText query "token"
            imageId <- getInteger query "minor_photo_id"
            Right (draftId, token, imageId)
    Logger.debug logger $ "Tried to parse query and got: " ++ show info
    case info of 
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right (draftId, token, imageId) -> do 
            result <- Handler.deleteMinorPhoto handle draftId token imageId
            Logger.debug logger $ "Tried to delete minor photo to draft and got: " ++ show result
            case result of 
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right _ -> return $ responseLBS status204 [] ""

handle :: Handler.Handle IO
handle = Handler.Handle {
    Handler.hCreate = manage . Db.Draft.create,
    Handler.hEditCategoryId = \a b -> manage $ Db.Draft.editCategoryId a b,
    Handler.hEditTagId = \a b -> manage $ Db.Draft.editTagId a b,
    Handler.hEditName = \a b -> manage $ Db.Draft.editName a b,
    Handler.hEditDescription = \a b -> manage $ Db.Draft.editDescription a b,
    Handler.hEditMainPhoto = \a b -> manage $ Db.Draft.editMainPhoto a b,
    Handler.hDoesExist = manage . Db.Draft.doesExist,
    Handler.hIsAuthor = manage . Db.Author.isAuthor,
    Handler.hHasPost = manage . Db.Draft.hasPost,
    Handler.hDelete = manage . Db.Draft.delete,
    Handler.hGetCurrentDate = do
        show <$> getCurrentTime,
    Handler.hGet = manage . Db.Draft.get,
    Handler.hGetAuthor = manage . Db.Author.getByDraftId,
    Handler.hPublish = \a b -> manage $ Db.Draft.publish a b,
    Handler.hUpdate = manage . Db.Draft.update,
    Handler.hGetAuthorIdByToken = manage . Db.Author.getByToken,
    Handler.hAddMinorPhoto = \a b -> manage $ Db.Draft.addMinorPhoto a b,
    Handler.hDeleteMinorPhoto = \a b -> manage $ Db.Draft.deleteMinorPhoto a b,
    Handler.hDoesAuthorExist = manage . Db.Author.doesExist,
    Handler.hDoesCategoryExist = manage . Db.Category.doesExist,
    Handler.hDoesTagExist = manage . Db.Tag.doesExist
}