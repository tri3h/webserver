{-# LANGUAGE OverloadedStrings #-}

module Post where

import Data.Maybe (fromMaybe)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Queries.Author as AuthorDb
import qualified Database.Queries.Category as CategoryDb
import qualified Database.Queries.Comment as CommentDb
import qualified Database.Queries.Post as Db
import qualified Database.Queries.Tag as TagDb
import qualified Database.Queries.User as UserDb
import qualified Handlers.Logger as Logger
import qualified Handlers.Post as Handler
import Network.HTTP.Types.URI (QueryText)
import Network.Wai (Response)
import qualified Types.Author as Author
import qualified Types.Category as Category
import Types.Config (ServerAddress)
import Types.Filter (postsOnPage)
import qualified Types.Filter as F
import Types.Post (Date (Date), Name (Name))
import qualified Types.Tag as Tag
import qualified Types.User as User
import Utility (getMaybeInteger, getMaybeIntegers, getMaybeText, response200JSON, response400)

get :: Logger.Handle IO -> Pool Connection -> ServerAddress -> QueryText -> IO Response
get logger pool address query = do
  let filters = getFilter query
  Logger.debug logger $ "Tried to parse query and got filters: " ++ show filters
  let order = getOrder query
  Logger.debug logger $ "Tried to parse query and got order: " ++ show order
  let limit = getLimit query
  let offset = getOffset query
  posts <- Handler.get (handle pool) address filters order limit offset
  Logger.debug logger $ "Tried to get posts and got: " ++ show posts
  return $ case posts of
    Left l -> response400 l
    Right r -> response200JSON r

getOffset :: QueryText -> F.Offset
getOffset query = F.Offset $ fromMaybe 0 (getMaybeInteger query "offset")

getLimit :: QueryText -> F.Limit
getLimit query = case getMaybeInteger query "limit" of
  Nothing -> postsOnPage
  Just x ->
    let limit = F.Limit x
     in if limit <= postsOnPage then limit else postsOnPage

getOrder :: QueryText -> F.Order
getOrder query = case getMaybeText query "sort_by" of
  Just "by_date" -> F.ByDate
  Just "by_author" -> F.ByAuthor
  Just "by_category" -> F.ByCategory
  Just "by_photos_number" -> F.ByPhotosNumber
  _ -> F.None

getFilter :: QueryText -> F.Filter
getFilter query =
  F.Filter
    { F.dateAfter = Date <$> getMaybeText query "date_after",
      F.dateBefore = Date <$> getMaybeText query "date_before",
      F.dateAt = Date <$> getMaybeText query "date_at",
      F.authorName = User.Name <$> getMaybeText query "author_name",
      F.categoryId = Category.CategoryId <$> getMaybeInteger query "category_id",
      F.tagId = Tag.TagId <$> getMaybeInteger query "tag_id",
      F.tag = Tag.Name <$> getMaybeText query "tag",
      F.tagIn = map Tag.TagId $ getMaybeIntegers query "tag_in",
      F.tagAll = map Tag.TagId $ getMaybeIntegers query "tag_all",
      F.postName = Name <$> getMaybeText query "post_name",
      F.text = getMaybeText query "text",
      F.substring = getMaybeText query "substring"
    }

handle :: Pool Connection -> Handler.Handle IO
handle pool =
  let f = withResource pool
   in Handler.Handle
        { Handler.hGet = \a b c d e -> f $ Db.get a b c d e,
          Handler.hGetMinorPhotos = \a b -> f $ Db.getMinorPhotos a b,
          Handler.hGetAuthor = f . AuthorDb.getMaybe . fromMaybe (Author.AuthorId 0),
          Handler.hGetUser = f . UserDb.getPostUser . fromMaybe (User.UserId 0),
          Handler.hGetCategory = f . CategoryDb.getWithParents,
          Handler.hGetTag = f . TagDb.getByPostId,
          Handler.hGetComment = f . CommentDb.get
        }
