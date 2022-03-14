module Types.Filter where

import Data.Text

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
}

data Order = Order {
    date :: Bool,
    author :: Bool,
    category :: Bool,
    photosNumber :: Bool
}