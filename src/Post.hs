{-# LANGUAGE OverloadedStrings #-}

module Post where

import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, withResource)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Author as AuthorDb
import qualified Database.Queries.Category as CategoryDb
import qualified Database.Queries.Comment as CommentDb
import qualified Database.Queries.Post as Db
import qualified Database.Queries.Tag as TagDb
import qualified Database.Queries.User as UserDb
import qualified Handlers.Logger as Logger
import qualified Handlers.Post as Handler
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (status200, status400)
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response, responseLBS)
import Types.Config (ServerAddress)
import qualified Types.Filter as F
import Types.Post (postsOnPage)
import Utility (getMaybeInteger, getMaybeIntegers, getMaybeText)

get :: Logger.Handle IO -> Pool Connection -> ServerAddress -> QueryText -> IO Response
get logger pool address query = do
  let filters =
        F.Filter
          { F.dateAfter = getMaybeText query "date_after",
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
  Logger.debug logger $ "Tried to parse query and got filters: " ++ show filters
  let order = case getMaybeText query "sort_by" of
        Just "by_date" -> F.ByDate
        Just "by_author" -> F.ByAuthor
        Just "by_category" -> F.ByCategory
        Just "by_photos_number" -> F.ByPhotosNumber
        _ -> F.None
  Logger.debug logger $ "Tried to parse query and got order: " ++ show order
  let limit = case getMaybeInteger query "limit" of
        Nothing -> postsOnPage
        Just x -> if x <= postsOnPage then x else postsOnPage
  let offset = fromMaybe 0 (getMaybeInteger query "offset")
  posts <- Handler.get (handle pool) address filters order limit offset
  Logger.debug logger $ "Tried to get posts and got: " ++ show posts
  case posts of
    Left l ->
      return $
        responseLBS
          status400
          []
          . encodeUtf8
          $ LazyText.fromStrict l
    Right r ->
      return $
        responseLBS
          status200
          [(hContentType, "application/json")]
          $ encode r

handle :: Pool Connection -> Handler.Handle IO
handle pool =
  Handler.Handle
    { Handler.hFilterHandle = filterHandle pool,
      Handler.hOrderHandle = orderHandle pool,
      Handler.hGet = withResource pool . Db.get,
      Handler.hGetAll = withResource pool Db.getAll,
      Handler.hGetMinorPhotos = withResource pool . Db.getMinorPhotos,
      Handler.hGetAuthor = withResource pool . AuthorDb.getMaybe,
      Handler.hGetUser = withResource pool . UserDb.getMaybeByUserId,
      Handler.hGetCategory = \catId -> do
        parents <- withResource pool $ CategoryDb.getParents catId
        mapM (withResource pool . CategoryDb.get) parents,
      Handler.hGetTag = withResource pool . TagDb.getByPostId,
      Handler.hGetComment = withResource pool . CommentDb.get,
      Handler.applyLimitOffset = \a b c -> withResource pool $ Db.applyLimitOffset a b c
    }

filterHandle :: Pool Connection -> Handler.FilterHandle IO
filterHandle pool =
  Handler.FilterHandle
    { Handler.hByDateBefore = withResource pool . Db.filterByDateBefore,
      Handler.hByDateAfter = withResource pool . Db.filterByDateAfter,
      Handler.hByDateAt = withResource pool . Db.filterByDateAt,
      Handler.hByAuthorName = withResource pool . Db.filterByAuthorName,
      Handler.hByCategoryId = withResource pool . Db.filterByCategoryId,
      Handler.hByTagId = withResource pool . Db.filterByTagId,
      Handler.hByTag = withResource pool . Db.filterByTag,
      Handler.hByOneOfTags = withResource pool . Db.filterByOneOfTags,
      Handler.hByAllOfTags = withResource pool . Db.filterByAllOfTags,
      Handler.hByPostName = withResource pool . Db.filterByPostName,
      Handler.hByText = withResource pool . Db.filterByText,
      Handler.hBySubstring = withResource pool . Db.filterBySubstring
    }

orderHandle :: Pool Connection -> Handler.OrderHandle IO
orderHandle pool =
  Handler.OrderHandle
    { Handler.hByDate = withResource pool . Db.orderByDate,
      Handler.hByAuthor = withResource pool . Db.orderByAuthor,
      Handler.hByCategory = withResource pool . Db.orderByCategory,
      Handler.hByPhotosNumber = withResource pool . Db.orderByPhotosNumber
    }
