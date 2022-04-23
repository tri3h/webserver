{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Post where

import Data.Maybe (fromMaybe)
import Data.Text (Text, append, pack)
import Database.PostgreSQL.Simple
  ( Connection,
    In (In),
    Only (Only, fromOnly),
    query,
  )
import qualified Database.PostgreSQL.Simple.Time as Time
import Types.Filter (Limit, Offset)
import Types.Image (Image (Id))
import Types.Post
  ( Post
      ( ShortPost,
        authorId,
        categoryId,
        date,
        mainPhoto,
        name,
        postId,
        text
      ),
    PostId,
    postNotExist,
  )
import qualified Types.Tag as Tag

get :: [PostId] -> Connection -> IO [Post]
get pId conn = do
  result <-
    query
      conn
      "SELECT p.post_id, p.author_id, p.category_id, \
      \p.name, p.date, p.text, p.image_id \
      \FROM posts p WHERE p.post_id IN ?"
      (Only $ In pId)
  let posts =
        map
          ( \( postId,
               maybeAuthorId,
               maybeCategoryId,
               name,
               date,
               text,
               mainPhoto
               ) ->
                ShortPost
                  { date = pack $ show (date :: Time.Date),
                    mainPhoto = Id mainPhoto,
                    authorId = fromMaybe 0 maybeAuthorId,
                    categoryId = fromMaybe 0 maybeCategoryId,
                    ..
                  }
          )
          result
  return posts

getAll :: Connection -> IO [PostId]
getAll conn = do
  result <- query conn "SELECT post_id FROM posts p" ()
  return $ map (\(Only x) -> x) result

applyLimitOffset :: [PostId] -> Limit -> Offset -> Connection -> IO [PostId]
applyLimitOffset posts limit offset conn = do
  result <- query conn "SELECT post_id FROM posts WHERE post_id IN ? LIMIT ? OFFSET ?" (In posts, limit, offset)
  return $ map (\(Only x) -> x) result

getMinorPhotos :: PostId -> Connection -> IO [Image]
getMinorPhotos postId conn = do
  xs <-
    query
      conn
      "SELECT image_id FROM post_minor_photos \
      \WHERE post_id = ?"
      (Only postId)
  let xs' =
        map
          ( \(Only x) -> Id x
          )
          xs
  return xs'

filterByDateBefore :: Text -> Connection -> IO [PostId]
filterByDateBefore date conn = do
  xs <- query conn "SELECT post_id FROM posts WHERE date < ?" $ Only date
  return $ map fromOnly xs

filterByDateAfter :: Text -> Connection -> IO [PostId]
filterByDateAfter date conn = do
  xs <- query conn "SELECT post_id FROM posts WHERE date > ?" $ Only date
  return $ map fromOnly xs

filterByDateAt :: Text -> Connection -> IO [PostId]
filterByDateAt date conn = do
  xs <- query conn "SELECT post_id FROM posts WHERE date = ?" $ Only date
  return $ map fromOnly xs

filterByAuthorName :: Text -> Connection -> IO [PostId]
filterByAuthorName name conn = do
  xs <-
    query
      conn
      "SELECT post_id FROM posts p INNER JOIN authors a \
      \ON p.author_id = a.author_id INNER JOIN users u ON \
      \u.user_id = a.user_id WHERE u.name = ?"
      $ Only name
  return $ map fromOnly xs

filterByCategoryId :: Integer -> Connection -> IO [PostId]
filterByCategoryId catId conn = do
  xs <-
    query
      conn
      "SELECT post_id FROM posts p INNER JOIN categories c \
      \ON p.category_id = c.category_id WHERE p.category_id = ?"
      $ Only catId
  return $ map fromOnly xs

filterByTagId :: Integer -> Connection -> IO [PostId]
filterByTagId tagId conn = do
  xs <- query conn "SELECT post_id FROM post_tags WHERE tag_id = ?" $ Only tagId
  return $ map fromOnly xs

filterByTag :: Text -> Connection -> IO [PostId]
filterByTag name conn = do
  xs <-
    query
      conn
      "SELECT post_id FROM post_tags pt INNER JOIN tags t \
      \ON pt.tag_id = t.tag_id WHERE t.name = ?"
      $ Only name
  return $ map fromOnly xs

filterByOneOfTags :: [Tag.TagId] -> Connection -> IO [PostId]
filterByOneOfTags tagId conn = do
  xs <-
    query
      conn
      "SELECT DISTINCT post_id FROM post_tags \
      \WHERE tag_id IN ? "
      (Only $ In tagId)
  return $ map fromOnly xs

filterByAllOfTags :: [Tag.TagId] -> Connection -> IO [PostId]
filterByAllOfTags tagId conn = do
  xs <-
    query
      conn
      "SELECT post_id FROM post_tags WHERE tag_id IN ? \
      \GROUP BY post_id HAVING COUNT(*) = ?"
      (In tagId, length tagId)
  return $ map fromOnly xs

filterByPostName :: Text -> Connection -> IO [PostId]
filterByPostName name conn = do
  let fullName = "%" `append` name `append` "%"
  xs <- query conn "SELECT post_id FROM posts WHERE name LIKE ?" $ Only fullName
  return $ map fromOnly xs

filterByText :: Text -> Connection -> IO [PostId]
filterByText name conn = do
  let fullName = "%" `append` name `append` "%"
  xs <- query conn "SELECT post_id FROM posts WHERE text LIKE ?" $ Only fullName
  return $ map fromOnly xs

filterBySubstring :: Text -> Connection -> IO [PostId]
filterBySubstring s conn = do
  let s' = "%" `append` s `append` "%"
  xs <-
    query
      conn
      "SELECT p.post_id FROM posts p INNER JOIN authors a \
      \ON p.author_id = a.author_id INNER JOIN users u \
      \ON u.user_id = a.user_id INNER JOIN categories c \
      \ON c.category_id = p.category_id INNER JOIN post_tags pt \
      \ON pt.post_id = p.post_id INNER JOIN tags t \
      \ON t.tag_id = pt.tag_id WHERE p.text LIKE ? \
      \OR u.name LIKE ? OR c.name LIKE ? OR t.name LIKE ?"
      (s', s', s', s')
  return $ map fromOnly xs

orderByDate :: [PostId] -> Connection -> IO [PostId]
orderByDate postId conn = do
  xs <-
    query
      conn
      "SELECT post_id FROM posts \
      \WHERE post_id IN ? ORDER BY date ASC"
      (Only $ In postId)
  return $ map fromOnly xs

orderByAuthor :: [PostId] -> Connection -> IO [PostId]
orderByAuthor postId conn = do
  xs <-
    query
      conn
      "SELECT post_id FROM posts p INNER JOIN authors a \
      \ON p.author_id = a.author_id INNER JOIN users u \
      \ON u.user_id = a.user_id WHERE p.post_id IN ? \
      \ORDER BY u.name ASC"
      (Only $ In postId)
  return $ map fromOnly xs

orderByCategory :: [PostId] -> Connection -> IO [PostId]
orderByCategory postId conn = do
  xs <-
    query
      conn
      "SELECT post_id FROM posts p INNER JOIN categories c \
      \ON p.category_id = c.category_id WHERE p.post_id IN ? \
      \ORDER BY c.name ASC"
      (Only $ In postId)
  return $ map fromOnly xs

orderByPhotosNumber :: [PostId] -> Connection -> IO [PostId]
orderByPhotosNumber postId conn = do
  xs <-
    query
      conn
      "WITH count_photos AS (SELECT p.post_id, \
      \COUNT(mp.image_id) FROM posts p INNER JOIN post_minor_photos mp \
      \ON p.post_id = mp.post_id WHERE p.post_id IN ? \
      \GROUP BY p.post_id ORDER BY COUNT(mp.image_id)) \
      \SELECT post_id FROM count_photos"
      (Only $ In postId)
  return $ map fromOnly xs

doesExist :: PostId -> Connection -> IO (Either Text ())
doesExist postId conn = do
  [Only n] <-
    query
      conn
      "SELECT COUNT(post_id) FROM posts \
      \WHERE posts.post_id = ?"
      (Only postId)
  if (n :: Integer) == 1
    then return $ Right ()
    else return $ Left postNotExist
