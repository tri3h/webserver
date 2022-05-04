{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Post (get, Handle (..)) where

import Data.List (zipWith7)
import Data.Text (Text)
import qualified Types.Author as A
import Types.Category (CategoryId, GetCategory)
import Types.Comment (GetComment)
import Types.Config (ServerAddress)
import qualified Types.Filter as F
import Types.Image (ImageId, Link)
import Types.Post
  ( FullPost (..),
    ShortPost (..),
    noPost,
  )
import Types.PostComment (PostId)
import qualified Types.Tag as Tag
import qualified Types.User as User
import Utility (imageIdToLink)

data Handle m = Handle
  { hGet :: F.Filter -> F.Order -> F.Limit -> F.Offset -> (ImageId -> Link) -> m [ShortPost],
    hGetMinorPhotos :: PostId -> (ImageId -> Link) -> m [Link],
    hGetAuthor :: Maybe A.AuthorId -> m (Maybe A.GetAuthor),
    hGetUser :: Maybe User.UserId -> (ImageId -> Link) -> m (Maybe User.GetUser),
    hGetCategory :: Maybe CategoryId -> m [GetCategory],
    hGetTag :: PostId -> m [Tag.Tag],
    hGetComment :: PostId -> m [GetComment]
  }

get :: Monad m => Handle m -> ServerAddress -> F.Filter -> F.Order -> F.Limit -> F.Offset -> m (Either Text [FullPost])
get handle server filters order limit offset = do
  let f = imageIdToLink server
  shortPosts <- hGet handle filters order limit offset f
  authors <- mapM (hGetAuthor handle . sAuthorId) shortPosts
  users <- mapM (\case Nothing -> return Nothing; Just a -> hGetUser handle (A.gUserId a) f) authors
  categories <- mapM (hGetCategory handle . sCategoryId) shortPosts
  tags <- mapM (hGetTag handle . sPostId) shortPosts
  comments <- mapM (hGetComment handle . sPostId) shortPosts
  minorPhotos <- mapM (\x -> hGetMinorPhotos handle (sPostId x) f) shortPosts
  let fullPosts =
        zipWith7
          ( \post fAuthor fUser fCategory fTag fComment fMinorPhoto ->
              FullPost
                { fPostId = sPostId post,
                  fName = sName post,
                  fDate = sDate post,
                  fText = sText post,
                  fMainPhoto = sMainPhoto post,
                  ..
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
