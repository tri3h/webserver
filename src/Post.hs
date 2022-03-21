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


get :: Query -> IO Response
get query = do 
    let res = queryToMaybeList query ["date_after", "date_before","date_at",
            "author_name","category_id","tag_id","tag","tag_in","tag_all", 
            "post_name","text", "substring", "order_date", "order_author",
            "order_category","order_photos_number"]
    let filter = F.Filter {
        F.dateAfter = head res,
        F.dateBefore = res !! 1,
        F.dateAt = res !! 2,
        F.authorName = res !! 3,
        F.categoryId = toInteger $ res !! 4,
        F.tagId = toInteger $ res !! 5,
        F.tag = res !! 6,
        F.tagIn = toIntegerList $ res !! 7,
        F.tagAll = toIntegerList $ res !! 8,
        F.postName = res !! 9,
        F.text = res !! 10,
        F.substring = res !! 11
    }
    let order = F.Order {
        F.date = toBool (res !! 12),
        F.author = toBool (res !! 13),
        F.category = toBool (res !! 14),
        F.photosNumber = toBool (res !! 15)
    }
    posts <- Handler.getPost handle filter order
    case posts of 
        Left l -> return $ responseLBS status400 [] . encodeUtf8 $ LazyText.fromStrict l
        Right r -> return $ responseLBS status200 [(hContentType, "application/json")] $ encode r
    where toBool Nothing = False
          toBool (Just a) = read (unpack a) :: Bool
          toInteger x = fmap (\a -> read (unpack a) :: Integer ) x
          toIntegerList x = fmap (\a -> read (unpack a) :: [Integer] ) x

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

