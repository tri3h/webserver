{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Database.Queries.Draft where

import Data.Text (Text)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    execute,
    executeMany,
    query,
  )
import Types.Category (CategoryId)
import Types.Draft (CreateDraft (..), DraftId, GetDraft (..), Name, draftNotExist)
import Types.Image (Image (..), ImageId)
import Types.Tag (TagId)

create :: CreateDraft -> Connection -> IO ()
create draft conn = do
  let (Image image imageType) = cMainPhoto draft
  [Only imageId] <-
    query
      conn
      "INSERT INTO images (image, image_type) \
      \VALUES (decode(?,'base64'), ?) RETURNING image_id"
      (image, imageType)
  [Only draftId] <-
    query
      conn
      "INSERT INTO drafts \
      \(category_id, name, text, image_id, author_id) VALUES (?,?,?,?,?) \
      \RETURNING draft_id"
      (cCategoryId draft, cName draft, cText draft, imageId :: Integer, cAuthorId draft)
  let tags = map (draftId,) $ cTagId draft
  _ <-
    executeMany
      conn
      "INSERT INTO draft_tags (draft_id, tag_id) \
      \VALUES (?,?)"
      (tags :: [(Integer, TagId)])
  return ()

get :: DraftId -> Connection -> IO GetDraft
get gDraftId conn = do
  [(gPostId, gAuthorId, gCategoryId, gName, gText, mainPhotoId)] <-
    query
      conn
      "SELECT post_id, author_id, category_id, name, \
      \text, image_id FROM drafts WHERE draft_id = ?"
      (Only gDraftId)
  tagIds <- query conn "SELECT tag_id FROM draft_tags WHERE draft_id = ?" (Only gDraftId)
  minorPhotoId <-
    query
      conn
      "SELECT image_id \
      \FROM draft_minor_photos WHERE draft_id = ?"
      (Only gDraftId)
  return $
    GetDraft
      { gTagId = map (\(Only x) -> x) tagIds,
        gMainPhoto = Id mainPhotoId,
        gMinorPhoto =
          map
            ( \(Only x) -> Id x
            )
            minorPhotoId,
        ..
      }

editCategoryId :: DraftId -> CategoryId -> Connection -> IO ()
editCategoryId draftId catId conn = do
  _ <- execute conn "UPDATE drafts SET category_id = ? WHERE draft_id = ?" (catId, draftId)
  return ()

editTagId :: DraftId -> [TagId] -> Connection -> IO ()
editTagId draftId tagIds conn = do
  let draftTags = map (draftId,) tagIds
  _ <-
    execute
      conn
      "DELETE FROM draft_tags dt INNER JOIN drafts d \
      \ON dt.draft_id = d.draft_id WHERE draft_id = ?"
      (Only draftId)
  _ <- executeMany conn "INSERT INTO draft_tags (draft_id, tag_id) VALUES (?,?)" draftTags
  return ()

editName :: DraftId -> Name -> Connection -> IO ()
editName draftId name conn = do
  _ <- execute conn "UPDATE drafts SET name = ? WHERE draft_id = ?" (name, draftId)
  return ()

editText :: DraftId -> Text -> Connection -> IO ()
editText draftId text conn = do
  _ <- execute conn "UPDATE drafts SET text = ? WHERE draft_id = ?" (text, draftId)
  return ()

editMainPhoto :: DraftId -> Image -> Connection -> IO ()
editMainPhoto draftId photo conn = do
  let (Image image imageType) = photo
  [Only imageId] <-
    query
      conn
      "INSERT INTO images (image, image_type) \
      \VALUES (decode(?,'base64'), ?) RETURNING image_id"
      (image, imageType)
  _ <-
    execute
      conn
      "UPDATE drafts SET image_id = ? \
      \WHERE draft_id = ?"
      (imageId :: Integer, draftId)
  return ()

deleteMinorPhoto :: DraftId -> ImageId -> Connection -> IO ()
deleteMinorPhoto draftId photoId conn = do
  _ <-
    execute
      conn
      "DELETE FROM draft_minor_photos \
      \WHERE image_id = ? AND draft_id = ?"
      (photoId, draftId)
  return ()

addMinorPhoto :: DraftId -> Image -> Connection -> IO ()
addMinorPhoto draftId photo conn = do
  let (Image image imageType) = photo
  [Only imageId] <-
    query
      conn
      "INSERT INTO images (image, image_type) \
      \VALUES (decode(?,'base64'), ?) RETURNING image_id"
      (image, imageType)
  _ <-
    execute
      conn
      "INSERT INTO draft_minor_photos (image_id, draft_id) \
      \VALUES (?,?)"
      (imageId :: Integer, draftId)
  return ()

delete :: DraftId -> Connection -> IO ()
delete draftId conn = do
  _ <- execute conn "DELETE FROM drafts WHERE drafts.draft_id = ?" (Only draftId)
  return ()

publish :: GetDraft -> String -> Connection -> IO ()
publish draft date conn = do
  [Only postId] <-
    query
      conn
      "INSERT INTO posts (author_id, category_id, \
      \name, date, text) VALUES (?,?,?,?,?) RETURNING post_id"
      ( gAuthorId draft,
        gCategoryId draft,
        gName draft,
        date,
        gText draft
      )
  [Only mainPhotoId] <- query conn "SELECT image_id FROM drafts WHERE draft_id = ?" (Only $ gDraftId draft)
  _ <- execute conn "UPDATE posts SET image_id = ? WHERE post_id = ?" (mainPhotoId :: Integer, postId)
  photoIds <- query conn "SELECT image_id FROM draft_minor_photos WHERE draft_id = ?" (Only $ gDraftId draft)
  let postPhotos = map (\(Only y) -> (postId :: Integer, y)) (photoIds :: [Only Integer])
  _ <-
    executeMany
      conn
      "INSERT INTO post_minor_photos (post_id, image_id) \
      \VALUES (?,?)"
      postPhotos
  let postTags = map (postId,) $ gTagId draft
  _ <- executeMany conn "INSERT INTO post_tags (post_id, tag_id) VALUES (?,?)" postTags
  _ <- execute conn "UPDATE drafts SET post_id = ? WHERE draft_id = ?" (postId, gDraftId draft)
  return ()

update :: GetDraft -> Connection -> IO ()
update draft conn = do
  _ <- execute conn "DELETE FROM post_tags WHERE post_id = ?" (Only $ gPostId draft)
  _ <- execute conn "DELETE FROM post_minor_photos WHERE post_id = ?" (Only $ gPostId draft)
  photoIds <- query conn "SELECT image_id FROM draft_minor_photos WHERE draft_id = ?" (Only $ gDraftId draft)
  let postPhotos = map (\(Only y) -> (gPostId draft, y)) (photoIds :: [Only Integer])
  _ <-
    executeMany
      conn
      "INSERT INTO post_minor_photos (post_id, image_id) \
      \VALUES (?,?)"
      postPhotos
  let postTags = map (gPostId draft,) $ gTagId draft
  _ <- executeMany conn "INSERT INTO post_tags (post_id, tag_id) VALUES (?,?)" postTags
  [Only mainPhotoId] <- query conn "SELECT image_id FROM drafts WHERE draft_id = ?" (Only $ gDraftId draft)
  _ <-
    execute
      conn
      "UPDATE posts SET category_id = ?, \
      \name = ?, text = ?, image_id = ? WHERE post_id = ?"
      (gCategoryId draft, gName draft, gText draft, mainPhotoId :: Integer, gPostId draft)
  return ()

hasPost :: DraftId -> Connection -> IO Bool
hasPost draftId conn = do
  [Only n] <- query conn "SELECT COUNT(post_id) FROM drafts WHERE draft_id = ?" (Only draftId)
  return $ (n :: Integer) == 1

doesExist :: DraftId -> Connection -> IO (Either Text ())
doesExist draftId conn = do
  [Only n] <- query conn "SELECT COUNT(draft_id) FROM drafts WHERE draft_id = ?" (Only draftId)
  if (n :: Integer) == 1
    then return $ Right ()
    else return $ Left draftNotExist
