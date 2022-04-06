module Types.Filter where

import Data.Text ( Text )

type Offset = Integer 
type Limit = Integer

data Filter = Filter {
    dateAfter :: Maybe Text,
    dateBefore :: Maybe Text,
    dateAt :: Maybe Text,
    authorName :: Maybe Text,
    categoryId :: Maybe Integer,
    tagId :: Maybe Integer,
    tag :: Maybe Text,
    tagIn :: Maybe [Integer],
    tagAll :: Maybe [Integer],
    postName :: Maybe Text,
    text :: Maybe Text,
    substring :: Maybe Text
} deriving Show

data Order = ByDate | ByAuthor | ByCategory | ByPhotosNumber | None deriving Show
