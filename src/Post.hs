{-# LANGUAGE OverloadedStrings #-}
module Post where

import Utility
import Types.Post
import qualified Types.Filter as F
import qualified Handler.Post as Handler
import qualified Database.Queries.Post as Db
import qualified Database.Queries.Author as AuthorDb
import qualified Database.Queries.User as UserDb 
import qualified Database.Queries.Category as CategoryDb
import qualified Database.Queries.Tag as TagDb
import qualified Database.Queries.Comment as CommentDb
import Database.Connection
import Data.Aeson
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.URI
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text ( Text, unpack )


get :: QueryText -> IO Response
get query = do
    let filter = F.Filter {
        F.dateAfter = getMaybeText query "date_after",
        F.dateBefore = getMaybeText query "date_before",
        F.dateAt = getMaybeText query "date_at",
        F.authorName = getMaybeText query "author_name",
        F.categoryId = getMaybeInteger query "category_id",
        F.tagId = getMaybeInteger query "tag_id",
        F.tag = getMaybeText query "tag",
        F.tagIn = getMaybeIntegers query "tag_in",
        F.tagAll = getMaybeIntegers query "tag_all",
        F.postName = getMaybeText query "post_name",
        F.text = getMaybeText query "text",
        F.substring = getMaybeText query "substring"
    }
    let order = case getMaybeText query "sort_by" of 
            Just "by_date" -> F.ByDate 
            Just "by_author" -> F.ByAuthor
            Just "by_category" -> F.ByCategory 
            Just "by_photos_number" -> F.ByPhotosNumber
            _ -> F.None
    posts <- Handler.getPost handle filter order
    case posts of 
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right r -> return $ responseLBS status200 [(hContentType, "application/json")] $ encode r

handle :: Handler.Handle IO
handle = Handler.Handle {
    Handler.get = manage . Db.get,
    Handler.getMinorPhotos = manage . Db.getMinorPhotos,
    Handler.getAuthor = manage . AuthorDb.get,
    Handler.getUser = manage . UserDb.getByUserId,
    Handler.getCategory = \catId -> do 
        parents <- manage $ CategoryDb.getParents catId
        mapM (manage . CategoryDb.get) parents,
    Handler.getTag = manage . TagDb.getByPostId,
    Handler.getComment = manage . CommentDb.get,
    Handler.filterByDateBefore = manage . Db.filterByDateBefore,
    Handler.filterByDateAfter = manage . Db.filterByDateAfter,
    Handler.filterByDateAt = manage . Db.filterByDateAt,
    Handler.filterByAuthorName = manage . Db.filterByAuthorName,
    Handler.filterByCategoryId = manage . Db.filterByCategoryId,
    Handler.filterByTagId = manage . Db.filterByTagId,
    Handler.filterByTag = manage . Db.filterByTag,
    Handler.filterByOneOfTags = manage . Db.filterByOneOfTags,
    Handler.filterByAllOfTags = manage . Db.filterByAllOfTags,
    Handler.filterByPostName = manage . Db.filterByPostName,
    Handler.filterByText = manage . Db.filterByText,
    Handler.filterBySubstring = manage . Db.filterBySubstring,
    Handler.orderByDate = manage . Db.orderByDate,
    Handler.orderByAuthor = manage . Db.orderByAuthor,
    Handler.orderByCategory = manage . Db.orderByCategory,
    Handler.orderByPhotosNumber = manage . Db.orderByPhotosNumber
}

