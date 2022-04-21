{-# LANGUAGE OverloadedStrings #-}

module Post where

import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.Connection (manage)
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
import Types.Config (Config (database, server), ServerConfig (sAddress))
import qualified Types.Filter as F
import Types.Post (postsOnPage)
import Utility (getMaybeInteger, getMaybeIntegers, getMaybeText)

get :: Logger.Handle IO -> Config -> QueryText -> IO Response
get logger config query = do
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
  posts <- Handler.get (handle config) (sAddress $ server config) filters order limit offset
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

handle :: Config -> Handler.Handle IO
handle config =
  let db = database config
   in Handler.Handle
        { Handler.hFilterHandle = filterHandle config,
          Handler.hOrderHandle = orderHandle config,
          Handler.hGet = \a b c -> manage db $ Db.get a b c,
          Handler.hGetAll = manage db Db.getAll,
          Handler.hGetMinorPhotos = manage db . Db.getMinorPhotos,
          Handler.hGetAuthor = manage db . AuthorDb.getMaybe,
          Handler.hGetUser = manage db . UserDb.getMaybeByUserId,
          Handler.hGetCategory = \catId -> do
            parents <- manage db $ CategoryDb.getParents catId
            mapM (manage db . CategoryDb.get) parents,
          Handler.hGetTag = manage db . TagDb.getByPostId,
          Handler.hGetComment = manage db . CommentDb.get
        }

filterHandle :: Config -> Handler.FilterHandle IO
filterHandle config =
  let db = database config
   in Handler.FilterHandle
        { Handler.hByDateBefore = manage db . Db.filterByDateBefore,
          Handler.hByDateAfter = manage db . Db.filterByDateAfter,
          Handler.hByDateAt = manage db . Db.filterByDateAt,
          Handler.hByAuthorName = manage db . Db.filterByAuthorName,
          Handler.hByCategoryId = manage db . Db.filterByCategoryId,
          Handler.hByTagId = manage db . Db.filterByTagId,
          Handler.hByTag = manage db . Db.filterByTag,
          Handler.hByOneOfTags = manage db . Db.filterByOneOfTags,
          Handler.hByAllOfTags = manage db . Db.filterByAllOfTags,
          Handler.hByPostName = manage db . Db.filterByPostName,
          Handler.hByText = manage db . Db.filterByText,
          Handler.hBySubstring = manage db . Db.filterBySubstring
        }

orderHandle :: Config -> Handler.OrderHandle IO
orderHandle config =
  let db = database config
   in Handler.OrderHandle
        { Handler.hByDate = manage db . Db.orderByDate,
          Handler.hByAuthor = manage db . Db.orderByAuthor,
          Handler.hByCategory = manage db . Db.orderByCategory,
          Handler.hByPhotosNumber = manage db . Db.orderByPhotosNumber
        }
