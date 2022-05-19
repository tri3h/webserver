module Types.Filter where

import Data.Text (Text)
import qualified Types.Category as Category
import qualified Types.Post as Post
import qualified Types.Tag as Tag
import qualified Types.User as User

data Filter = Filter
  { dateAfter :: Maybe Post.Date,
    dateBefore :: Maybe Post.Date,
    dateAt :: Maybe Post.Date,
    authorName :: Maybe User.Name,
    categoryId :: Maybe Category.CategoryId,
    tagId :: Maybe Tag.TagId,
    tag :: Maybe Tag.Name,
    tagIn :: [Tag.TagId],
    tagAll :: [Tag.TagId],
    postName :: Maybe Post.Name,
    text :: Maybe Text,
    substring :: Maybe Text
  }
  deriving (Show)

data Order = ByDate | ByAuthor | ByCategory | ByPhotosNumber | None deriving (Show)
