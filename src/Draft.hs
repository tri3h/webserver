{-# LANGUAGE OverloadedStrings #-}
module Draft where

import Prelude hiding (words)
import Data.Text (Text, unpack, words)
import Data.Aeson
import Types.Draft
import Types.Author(AuthorId)
import Types.Category(CategoryId)
import Types.Tag(TagId)
import Types.Post(PostId, Date)
import Types.User(Token)
import qualified Handler.Draft as Handler
import qualified Database.Queries.Draft as Db.Draft
import qualified Database.Queries.Author as Db.Author
import Database.Connection
import Data.Time.Clock
import Network.HTTP.Types.URI ( QueryText )
import Control.Monad
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text.Encoding ( decodeUtf8, decodeUtf16BE )
import qualified Data.Text.Lazy as LazyText
import Utility

create :: QueryText -> IO Response
create query = do 
    let isDraft = getInteger query "category_id" >>= 
            \categoryId -> getIntegers query "tag_id" >>=
            \tagId -> getText query "name" >>=
            \name -> getText query "description" >>=
            \description -> getImage query "main_photo" >>=
            \mainPhoto -> getImages query "minor_photo" >>=
            \minorPhoto -> Right $ Draft {
                draftId = Nothing,
                postId = Nothing,
                authorId = undefined,
                categoryId = categoryId,
                tagId = tagId,
                name = name,
                description = description,
                mainPhoto = mainPhoto,
                minorPhoto = minorPhoto
                }
    case isDraft of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right draft -> do 
            Handler.create handle draft
            return $ responseLBS status200 [] ""

get :: QueryText -> IO Response
get query = do 
    case getInteger query "draft_id" >>=
        \draftId -> getText query "token" >>=
        \token -> Right (draftId, token) of 
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right (draftId, token) -> do
            result <- Handler.get handle draftId token
            case result of 
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right draft -> return $ responseLBS status200 [(hContentType, "application/json")] $ encode draft

delete :: QueryText -> IO Response
delete query = do 
    case getInteger query "draft_id" >>=
        \draftId -> getText query "token" >>=
        \token -> Right (draftId, token) of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right (draftId, token) -> do 
            result <- Handler.delete handle draftId token
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right _ -> return $ responseLBS status200 [] ""
            
edit :: QueryText -> IO Response
edit query = do 
    case getInteger query "draft_id" of 
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right draftId -> do 
            let editParams = EditParams {
                    eCategoryId = getMaybeInteger query "category_id" ,
                    eTagId = getMaybeIntegers query "tag_id",
                    eName = getMaybeText query "name",
                    eDescription = getMaybeText query "description",
                    eMainPhoto = getMaybeImage query "main_photo",
                    eMinorPhoto = getMaybeImages query "minor_photo"
                    }
            result <- Handler.edit handle draftId editParams
            case result of 
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right _ -> return $ responseLBS status200 [] ""

publish :: QueryText -> IO Response
publish query = do 
    case getInteger query "draft_id" >>=
        \draftId -> getText query "token" >>=
        \token -> Right (draftId, token) of
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right (draftId, token) -> do 
            result <- Handler.publish handle draftId token
            case result of
                Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
                Right postId -> return $ responseLBS status200 [(hContentType, "application/json")] $ encode postId

handle :: Handler.Handle IO
handle = Handler.Handle {
    Handler.hCreate = manage . Db.Draft.create,
    Handler.hEditCategoryId = \a b -> manage $ Db.Draft.editCategoryId a b,
    Handler.hEditTagId = \a b -> manage $ Db.Draft.editTagId a b,
    Handler.hEditName = \a b -> manage $ Db.Draft.editName a b,
    Handler.hEditDescription = \a b -> manage $ Db.Draft.editDescription a b,
    Handler.hEditMainPhoto = \a b -> manage $ Db.Draft.editMainPhoto a b,
    Handler.hEditMinorPhotos = \a b -> manage $ Db.Draft.editMinorPhotos a b,
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
    Handler.hGetAuthorIdByToken = manage . Db.Author.getByToken
}