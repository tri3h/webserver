{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Database.Queries.Draft where

import Control.Exception (try)
import Control.Monad (void)
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple
  ( Binary (Binary),
    Connection,
    Only (Only, fromOnly),
    SqlError (sqlErrorMsg),
    execute,
    executeMany,
    query,
  )
import Database.PostgreSQL.Simple.Transaction
  ( begin,
    commit,
    rollback,
  )
import Error (Error, categoryNotExist, draftNotExist, tagNotExist, unknownError, userNotAuthor)
import Types.Category (CategoryId)
import Types.Draft (CreateDraft (..), DraftId (DraftId), GetDraft (..), Name)
import Types.Image (Image (..), ImageId, Link)
import Types.Limit (Limit, Offset)
import Types.Tag (TagId)
import Types.User (Token)

create :: CreateDraft -> Connection -> IO (Either Error DraftId)
create draft conn = do
  begin conn
  resultDraft <- addDraft
  case resultDraft of
    Right r -> addTags r
    Left l -> do
      rollback conn
      return $ case sqlErrorMsg l of
        "insert or update on table \"drafts\" violates foreign key constraint \"category_id\"" -> Left categoryNotExist
        "null value in column \"author_id\" violates not-null constraint" -> Left userNotAuthor
        _ -> Left unknownError
  where
    addDraft =
      case cMainPhoto draft of
        Just (Image image imageType) ->
          try $
            query
              conn
              "WITH i AS (INSERT INTO images (image, image_type) VALUES (?,?) RETURNING image_id) \
              \INSERT INTO drafts (category_id, name, text, image_id, author_id) \
              \VALUES (?,?,?,(SELECT image_id FROM i),(SELECT author_id FROM authors a INNER JOIN users u \
              \ON a.user_id = u.user_id WHERE u.token = ?)) RETURNING draft_id"
              (Binary image, imageType, cCategoryId draft, cName draft, cText draft, cToken draft) ::
            IO (Either SqlError [Only Integer])
        Nothing ->
          try $
            query
              conn
              "INSERT INTO drafts (category_id, name, text, author_id) \
              \VALUES (?,?,?,(SELECT author_id FROM authors a INNER JOIN users u \
              \ON a.user_id = u.user_id WHERE u.token = ?)) RETURNING draft_id"
              (cCategoryId draft, cName draft, cText draft, cToken draft) ::
            IO (Either SqlError [Only Integer])
    addTags r = do
      let [Only draftId] = r
      let tags = map (draftId,) $ cTagId draft
      resultTag <-
        try . void $
          executeMany
            conn
            "INSERT INTO draft_tags (draft_id, tag_id) VALUES (?,?)"
            (tags :: [(Integer, TagId)]) ::
          IO (Either SqlError ())
      case resultTag of
        Right _ -> do
          commit conn
          return . Right $ DraftId draftId
        Left l -> do
          rollback conn
          return $ case sqlErrorMsg l of
            "insert or update on table \"draft_tags\" violates foreign key constraint \"tag_id\"" -> Left tagNotExist
            _ -> Left unknownError

get :: DraftId -> (ImageId -> Link) -> Connection -> IO GetDraft
get draftId f conn = do
  [(postId, authorId, categoryId, name, text, mainPhotoId)] <-
    query
      conn
      "SELECT post_id, author_id, category_id, name, \
      \text, image_id FROM drafts WHERE draft_id = ?"
      (Only draftId)
  tagIds <- query conn "SELECT tag_id FROM draft_tags WHERE draft_id = ?" (Only draftId)
  minorPhotoId <-
    query
      conn
      "SELECT image_id FROM draft_minor_photos WHERE draft_id = ?"
      (Only draftId)
  return $
    GetDraft
      { gTagId = map fromOnly tagIds,
        gMainPhoto = f <$> mainPhotoId,
        gMinorPhoto =
          map
            (f . fromOnly)
            minorPhotoId,
        gPostId = postId,
        gCategoryId = categoryId,
        gName = name,
        gText = text,
        gDraftId = draftId,
        gAuthorId = authorId
      }

getAllByAuthor :: Token -> Limit -> Offset -> Connection -> IO [DraftId]
getAllByAuthor token limit offset conn = do
  result <-
    query
      conn
      "SELECT draft_id FROM drafts d INNER JOIN authors a \
      \ON d.author_id = a.author_id INNER JOIN users u ON u.user_id = a.user_id \
      \WHERE token = ? LIMIT ? OFFSET ?"
      (token, limit, offset)
  return $ map fromOnly result

editCategoryId :: DraftId -> CategoryId -> Connection -> IO (Either Error ())
editCategoryId draftId catId conn = do
  result <- try $ execute conn "UPDATE drafts SET category_id = ? WHERE draft_id = ?" (catId, draftId) :: IO (Either SqlError Int64)
  case result of
    Right r -> return $ if r == 0 then Left draftNotExist else Right ()
    Left l -> return $ case sqlErrorMsg l of
      "insert or update on table \"drafts\" violates foreign key constraint \"category_id\"" -> Left categoryNotExist
      _ -> Left unknownError

editTagId :: DraftId -> [TagId] -> Connection -> IO (Either Error ())
editTagId draftId tagIds conn = do
  let draftTags = map (draftId,) tagIds
  begin conn
  void $
    execute
      conn
      "DELETE FROM draft_tags WHERE draft_id = ?"
      (Only draftId)
  result <-
    try $
      executeMany
        conn
        "INSERT INTO draft_tags (draft_id, tag_id) VALUES (?,?)"
        draftTags ::
      IO (Either SqlError Int64)
  case result of
    Right _ -> do
      commit conn
      return $ Right ()
    Left l -> do
      rollback conn
      return $ case sqlErrorMsg l of
        "insert or update on table \"draft_tags\" violates foreign key constraint \"draft_id\"" -> Left draftNotExist
        "insert or update on table \"draft_tags\" violates foreign key constraint \"tag_id\"" -> Left tagNotExist
        _ -> Left unknownError

editName :: DraftId -> Name -> Connection -> IO (Either Error ())
editName draftId name conn = do
  result <- execute conn "UPDATE drafts SET name = ? WHERE draft_id = ?" (name, draftId)
  return $ if result == 0 then Left draftNotExist else Right ()

editText :: DraftId -> Text -> Connection -> IO (Either Error ())
editText draftId text conn = do
  result <- execute conn "UPDATE drafts SET text = ? WHERE draft_id = ?" (text, draftId)
  return $ if result == 0 then Left draftNotExist else Right ()

editMainPhoto :: DraftId -> Image -> Connection -> IO (Either Error ())
editMainPhoto draftId photo conn = do
  let (Image image imageType) = photo
  result <-
    execute
      conn
      "WITH i AS (INSERT INTO images (image, image_type) \
      \VALUES (?,?) RETURNING image_id) \
      \UPDATE drafts SET image_id = (SELECT image_id FROM i) WHERE draft_id = ?"
      (Binary image, imageType, draftId)
  return $ if result == 0 then Left draftNotExist else Right ()

deleteMinorPhoto :: DraftId -> ImageId -> Connection -> IO (Either Error ())
deleteMinorPhoto draftId photoId conn = do
  result <-
    execute
      conn
      "DELETE FROM draft_minor_photos WHERE image_id = ? AND draft_id = ?"
      (photoId, draftId)
  return $ if result == 0 then Left draftNotExist else Right ()

addMinorPhoto :: DraftId -> Image -> Connection -> IO (Either Error ())
addMinorPhoto draftId photo conn = do
  let (Image image imageType) = photo
  result <-
    execute
      conn
      "WITH i AS (INSERT INTO images (image, image_type) \
      \VALUES (?, ?) RETURNING image_id) \
      \INSERT INTO draft_minor_photos (image_id, draft_id) \
      \VALUES ((SELECT image_id FROM i),?)"
      (Binary image, imageType, draftId)
  return $ if result == 0 then Left draftNotExist else Right ()

delete :: DraftId -> Connection -> IO (Either Error ())
delete draftId conn = do
  result <- execute conn "DELETE FROM drafts WHERE drafts.draft_id = ?" (Only draftId)
  return $ if result == 0 then Left draftNotExist else Right ()

publish :: DraftId -> Connection -> IO (Either Error ())
publish draftId conn = do
  result <-
    query
      conn
      "INSERT INTO posts (author_id, category_id, name, date, text, image_id) \
      \(SELECT author_id, category_id, name, CURRENT_DATE, text, image_id \
      \FROM drafts d WHERE draft_id = ?) RETURNING post_id"
      (Only draftId)
  if null result
    then return $ Left draftNotExist
    else do
      let [Only postId] = result
      void $ execute conn "UPDATE drafts SET post_id = ? WHERE draft_id = ?" (postId :: Integer, draftId)
      void $
        execute
          conn
          "INSERT INTO post_minor_photos (post_id, image_id) \
          \(SELECT d.post_id, dmp.image_id FROM draft_minor_photos dmp INNER JOIN \
          \drafts d ON dmp.draft_id = d.draft_id WHERE dmp.draft_id = ?)"
          (Only draftId)
      Right
        <$> void
          ( execute
              conn
              "INSERT INTO post_tags (post_id, tag_id) (SELECT d.post_id, dt.tag_id FROM draft_tags dt \
              \INNER JOIN drafts d ON dt.draft_id = d.draft_id WHERE dt.draft_id = ?)"
              (Only draftId)
          )

update :: DraftId -> Connection -> IO (Either Error ())
update draftId conn = do
  result <- query conn "SELECT post_id FROM drafts WHERE draft_id = ?" (Only draftId)
  if null result
    then return $ Left draftNotExist
    else do
      let [Only postId] = result
      void $ execute conn "DELETE FROM post_tags WHERE post_id = ?" (Only (postId :: Integer))
      void $ execute conn "DELETE FROM post_minor_photos WHERE post_id = ?" (Only postId)
      void $
        execute
          conn
          "INSERT INTO post_minor_photos (post_id, image_id) (SELECT d.post_id, dmp.image_id \
          \FROM draft_minor_photos dmp INNER JOIN drafts d ON dmp.draft_id = d.draft_id \
          \WHERE dmp.draft_id = ?)"
          (Only draftId)
      void $
        execute
          conn
          "INSERT INTO post_tags (post_id, tag_id) (SELECT d.post_id, dt.tag_id FROM draft_tags dt \
          \INNER JOIN drafts d ON dt.draft_id = d.draft_id WHERE dt.draft_id = ?)"
          (Only draftId)
      Right
        <$> void
          ( execute
              conn
              "UPDATE posts SET category_id = sub.category_id, name = sub.name, text = sub.text, \
              \image_id = sub.image_id FROM (SELECT category_id, name, text, image_id FROM drafts \
              \WHERE draft_id = ?) AS sub WHERE post_id = ?"
              (draftId, postId)
          )

hasPost :: DraftId -> Connection -> IO Bool
hasPost draftId conn = do
  [Only n] <- query conn "SELECT COUNT(post_id) FROM drafts WHERE draft_id = ?" (Only draftId)
  return $ (n :: Integer) == 1
