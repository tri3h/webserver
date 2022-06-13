{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Post (get, Handle (..)) where

import Data.List (zipWith7)
import Error (Error, noPost)
import qualified Handlers.Logger as Logger
import Network.HTTP.Types (QueryText)
import Network.Wai (Response)
import qualified Types.Author as A
import Types.Category (CategoryId, GetCategory)
import qualified Types.Category as Category
import Types.Comment (GetComment)
import Types.Config (ServerAddress)
import qualified Types.Filter as F
import Types.Image (ImageId, Link)
import Types.Limit (Limit, Offset (Offset))
import Types.Post
  ( Date (Date),
    FullPost (..),
    Name (Name),
    ShortPost (..),
    postsOnPage,
  )
import Types.PostComment (PostId)
import qualified Types.Tag as Tag
import qualified Types.User as User
import Utility (getLimit, getMaybeInteger, getMaybeIntegers, getMaybeText, getOffset, imageIdToLink, response200JSON, response400)

data Handle m = Handle
  { hGet :: F.Filter -> F.Order -> Limit -> Offset -> (ImageId -> Link) -> m [ShortPost],
    hGetMinorPhotos :: PostId -> (ImageId -> Link) -> m [Link],
    hGetAuthor :: Maybe A.AuthorId -> m (Maybe A.GetAuthor),
    hGetUser :: Maybe User.UserId -> m (Maybe User.PostUser),
    hGetCategory :: Maybe CategoryId -> m [GetCategory],
    hGetTag :: PostId -> m [Tag.Tag],
    hGetComment :: PostId -> m [GetComment]
  }

get :: Monad m => Handle m -> Logger.Handle m -> ServerAddress -> QueryText -> m Response
get handle logger address query = do
  let filters = getFilter query
  Logger.debug logger $ "Tried to parse query and got filters: " ++ show filters
  let order = getOrder query
  Logger.debug logger $ "Tried to parse query and got order: " ++ show order
  let limit = getLimit query postsOnPage
  let offset = getOffset query (Offset 0)
  posts <- getPost handle address filters order limit offset
  Logger.debug logger $ "Tried to get posts and got: " ++ show posts
  return $ case posts of
    Left l -> response400 l
    Right r -> response200JSON r

getPost :: Monad m => Handle m -> ServerAddress -> F.Filter -> F.Order -> Limit -> Offset -> m (Either Error [FullPost])
getPost handle server filters order limit offset = do
  let f = imageIdToLink server
  shortPosts <- hGet handle filters order limit offset f
  authors <- mapM (hGetAuthor handle . sAuthorId) shortPosts
  users <- mapM (\case Nothing -> return Nothing; Just a -> hGetUser handle (A.gUserId a)) authors
  categories <- mapM (hGetCategory handle . sCategoryId) shortPosts
  tags <- mapM (hGetTag handle . sPostId) shortPosts
  comments <- mapM (hGetComment handle . sPostId) shortPosts
  minorPhotos <- mapM (\x -> hGetMinorPhotos handle (sPostId x) f) shortPosts
  let fullPosts =
        zipWith7
          ( \post author user category tag comment minorPhoto ->
              FullPost
                { fPostId = sPostId post,
                  fName = sName post,
                  fDate = sDate post,
                  fText = sText post,
                  fMainPhoto = sMainPhoto post,
                  fAuthor = author,
                  fUser = user,
                  fComment = comment,
                  fCategory = category,
                  fTag = tag,
                  fMinorPhoto = minorPhoto
                }
          )
          shortPosts
          authors
          users
          categories
          tags
          comments
          minorPhotos
  return $
    if null fullPosts
      then Left noPost
      else Right fullPosts

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
