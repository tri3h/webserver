{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Post where

import qualified Data.Bifunctor
import Data.String (fromString)
import Data.Text (Text, append, pack)
import Database.PostgreSQL.Simple
  ( Connection,
    In (In),
    Only (Only, fromOnly),
    query,
  )
import qualified Database.PostgreSQL.Simple.Time as Time
import Database.PostgreSQL.Simple.ToField (Action, ToField (toField))
import qualified Types.Category as Category
import qualified Types.Filter as F
import Types.Image (ImageId, Link)
import Types.Limit (Limit, Offset)
import Types.Post
  ( Date (Date),
    Name (Name),
    ShortPost (..),
  )
import Types.PostComment (PostId)
import qualified Types.Tag as Tag
import qualified Types.User as User

type QueryPart = (String, [Action])

get :: F.Filter -> F.Order -> Limit -> Offset -> (ImageId -> Link) -> Connection -> IO [ShortPost]
get filters order limit offset f conn = do
  let (str, params) = makeQuery filters order limit offset
  result <- query conn (fromString str) params
  let posts =
        map
          ( \( sPostId,
               sAuthorId,
               sCategoryId,
               sName,
               date,
               sText,
               mainPhoto
               ) ->
                ShortPost
                  { sDate = Date . pack $ show (date :: Time.Date),
                    sMainPhoto = f <$> mainPhoto,
                    ..
                  }
          )
          result
  return posts

makeQuery :: F.Filter -> F.Order -> Limit -> Offset -> QueryPart
makeQuery filters order limit offset =
  let filters' = applyFilters filters
      limitOffset' = limitOffset limit offset
      order' = applyOrder order
   in if null filters'
        then
          Data.Bifunctor.first
            (withAs (selectAll ++ groupBy ++ order') ++)
            limitOffset'
        else
          let whereArg = foldl1 (\(str1, param1) (str2, param2) -> (str1 ++ andWord ++ str2, param1 ++ param2)) filters'
           in Data.Bifunctor.bimap
                ( withAs
                    (selectAll ++ whereWord ++ fst whereArg ++ groupBy ++ order')
                    ++
                )
                (snd whereArg ++)
                limitOffset'

applyFilters :: F.Filter -> [QueryPart]
applyFilters filters =
  concat
    [ case F.dateAfter filters of
        Just x -> [whereDateAfter x]
        Nothing -> [],
      case F.dateBefore filters of
        Just x -> [whereDateBefore x]
        Nothing -> [],
      case F.dateAt filters of
        Just x -> [whereDateAt x]
        Nothing -> [],
      case F.authorName filters of
        Just x -> [whereAuthorName x]
        Nothing -> [],
      case F.categoryId filters of
        Just x -> [whereCategoryId x]
        Nothing -> [],
      case F.tagId filters of
        Just x -> [whereTagId x]
        Nothing -> [],
      case F.tag filters of
        Just x -> [whereTag x]
        Nothing -> [],
      case F.tagIn filters of
        [] -> []
        x -> [whereTagIn x],
      case F.tagAll filters of
        [] -> []
        x -> [whereTagAll x],
      case F.postName filters of
        Just x -> [wherePostName x]
        Nothing -> [],
      case F.text filters of
        Just x -> [whereText x]
        Nothing -> [],
      case F.substring filters of
        Just x -> [whereSubstring x]
        Nothing -> []
    ]

applyOrder :: F.Order -> String
applyOrder order = case order of
  F.ByDate -> orderByDate
  F.ByAuthor -> orderByAuthor
  F.ByCategory -> orderByCategory
  F.ByPhotosNumber -> orderByPhotosNumber
  _ -> ""

withAs :: String -> String
withAs str =
  " WITH everything AS (" ++ str
    ++ ") SELECT post_id, author_id, category_id, \
       \name, date, text, image_id FROM everything "

selectAll :: String
selectAll =
  " SELECT DISTINCT p.post_id, p.author_id, p.category_id, \
  \p.name, p.date, p.text, p.image_id, u.name AS author_name, \
  \c.name AS category_name, COUNT(pmp.image_id) \
  \FROM posts p INNER JOIN authors a ON p.author_id = a.author_id \
  \INNER JOIN users u ON a.user_id = u.user_id \
  \INNER JOIN categories c ON c.category_id = p.category_id \
  \LEFT JOIN post_tags pt ON pt.post_id = p.post_id \
  \LEFT JOIN tags t ON t.tag_id = pt.tag_id \
  \LEFT JOIN post_minor_photos pmp ON pmp.post_id = p.post_id "

whereWord :: String
whereWord = " WHERE "

andWord :: String
andWord = " AND "

limitOffset :: Limit -> Offset -> QueryPart
limitOffset lim off = (" LIMIT ? OFFSET ? ", [toField lim, toField off])

whereDateBefore :: Date -> QueryPart
whereDateBefore date = (" p.date < ? ", [toField date])

whereDateAfter :: Date -> QueryPart
whereDateAfter date = (" p.date > ? ", [toField date])

whereDateAt :: Date -> QueryPart
whereDateAt date = (" p.date = ? ", [toField date])

whereAuthorName :: User.Name -> QueryPart
whereAuthorName name = (" u.name = ? ", [toField name])

whereCategoryId :: Category.CategoryId -> QueryPart
whereCategoryId categoryId = (" p.category_id = ? ", [toField categoryId])

whereTagId :: Tag.TagId -> QueryPart
whereTagId tagId = (" pt.tag_id = ? ", [toField tagId])

whereTag :: Tag.Name -> QueryPart
whereTag tag = (" t.name = ? ", [toField tag])

whereTagIn :: [Tag.TagId] -> QueryPart
whereTagIn tagId = (" pt.tag_id IN ? ", [toField $ In tagId])

whereTagAll :: [Tag.TagId] -> QueryPart
whereTagAll tagId =
  ( " p.post_id IN (SELECT post_id FROM post_tags WHERE tag_id IN ? \
    \GROUP BY post_id HAVING COUNT(*) = ?) ",
    [toField $ In tagId, toField $ length tagId]
  )

wherePostName :: Name -> QueryPart
wherePostName (Name name) =
  let fullName = "%" `append` name `append` "%"
   in (" p.name LIKE ? ", [toField fullName])

whereText :: Text -> QueryPart
whereText t =
  let fullText = "%" `append` t `append` "%"
   in (" p.text LIKE ? ", [toField fullText])

whereSubstring :: Text -> QueryPart
whereSubstring substr =
  let fullSubstr = "%" `append` substr `append` "%"
   in ( " (p.text LIKE ? OR u.name LIKE ? OR c.name LIKE ? OR t.name LIKE ?) ",
        replicate 4 (toField fullSubstr)
      )

orderByDate :: String
orderByDate = " ORDER BY p.date ASC "

orderByAuthor :: String
orderByAuthor = " ORDER BY u.name ASC "

orderByCategory :: String
orderByCategory = " ORDER BY c.name ASC "

orderByPhotosNumber :: String
orderByPhotosNumber =
  " ORDER BY COUNT(pmp.image_id) ASC "

groupBy :: String
groupBy =
  " GROUP BY p.post_id, p.author_id, p.category_id, \
  \p.name, p.date, p.text, p.image_id, author_name, \
  \category_name "

getMinorPhotos :: PostId -> (ImageId -> Link) -> Connection -> IO [Link]
getMinorPhotos postId f conn = do
  xs <-
    query
      conn
      "SELECT image_id FROM post_minor_photos \
      \WHERE post_id = ?"
      (Only postId)
  return $ map (f . fromOnly) xs
