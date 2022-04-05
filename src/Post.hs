{-# LANGUAGE OverloadedStrings #-}
module Post where

import Utility ( getMaybeText, getMaybeInteger, getMaybeIntegers )
import Types.Post ( postsOnPage )
import qualified Types.Filter as F
import qualified Handler.Post as Handler
import qualified Database.Queries.Post as Db
import qualified Database.Queries.Author as AuthorDb
import qualified Database.Queries.User as UserDb
import qualified Database.Queries.Category as CategoryDb
import qualified Database.Queries.Tag as TagDb
import qualified Database.Queries.Comment as CommentDb
import Database.Connection ( manage )
import Data.Aeson ( encode )
import Network.Wai ( Response, responseLBS )
import Network.HTTP.Types.Status ( status200, status400 )
import Network.HTTP.Types.Header ( hContentType )
import Network.HTTP.Types.URI ( QueryText )
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import Data.Text ( Text, unpack )
import Data.Maybe ( fromMaybe )

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
    let limit = case getMaybeInteger query "limit" of
                    Nothing -> postsOnPage
                    Just x -> if x <= postsOnPage then x else postsOnPage
    let offset = fromMaybe 0 (getMaybeInteger query "offset")
    posts <- Handler.get handle filter order limit offset
    case posts of
        Left l -> return $ responseLBS status400
            [] . encodeUtf8 $ LazyText.fromStrict l
        Right r -> return $ responseLBS status200
            [(hContentType, "application/json")] $ encode r

handle :: Handler.Handle IO
handle = Handler.Handle {
    Handler.hFilterHandle = filterHandle,
    Handler.hOrderHandle = orderHandle,
    Handler.hGet = \a b c -> manage $ Db.get a b c,
    Handler.hGetAll = manage Db.getAll,
    Handler.hGetMinorPhotos = manage . Db.getMinorPhotos,
    Handler.hGetAuthor = manage . AuthorDb.getMaybe,
    Handler.hGetUser = manage . UserDb.getMaybeByUserId,
    Handler.hGetCategory = \catId -> do
        parents <- manage $ CategoryDb.getParents catId
        mapM (manage . CategoryDb.get) parents,
    Handler.hGetTag = manage . TagDb.getByPostId,
    Handler.hGetComment = manage . CommentDb.get
}

filterHandle :: Handler.FilterHandle IO
filterHandle = Handler.FilterHandle {
    Handler.hByDateBefore = manage . Db.filterByDateBefore,
    Handler.hByDateAfter = manage . Db.filterByDateAfter,
    Handler.hByDateAt = manage . Db.filterByDateAt,
    Handler.hByAuthorName = manage . Db.filterByAuthorName,
    Handler.hByCategoryId = manage . Db.filterByCategoryId,
    Handler.hByTagId = manage . Db.filterByTagId,
    Handler.hByTag = manage . Db.filterByTag,
    Handler.hByOneOfTags = manage . Db.filterByOneOfTags,
    Handler.hByAllOfTags = manage . Db.filterByAllOfTags,
    Handler.hByPostName = manage . Db.filterByPostName,
    Handler.hByText = manage . Db.filterByText,
    Handler.hBySubstring = manage . Db.filterBySubstring
}

orderHandle :: Handler.OrderHandle IO
orderHandle = Handler.OrderHandle {
    Handler.hByDate = manage . Db.orderByDate,
    Handler.hByAuthor = manage . Db.orderByAuthor,
    Handler.hByCategory = manage . Db.orderByCategory,
    Handler.hByPhotosNumber = manage . Db.orderByPhotosNumber
}

