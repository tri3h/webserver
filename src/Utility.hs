{-# LANGUAGE OverloadedStrings #-}
module Utility(queryToList, queryToMaybeList) where

import Data.ByteString (ByteString)
import Network.HTTP.Types.URI
import Data.Text (Text)
import Control.Monad
import Data.Text.Encoding ( decodeUtf8 )

queryToList :: Query -> [ByteString] -> Either Text [Text]
queryToList query list = let arr = map (\a -> join $ lookup a query) list in 
    if Nothing `notElem` arr
        then Right $ map (decodeUtf8 . (\(Just a) -> a)) arr
        else Left "Not enough parameters"

queryToMaybeList :: Query -> [ByteString] -> [Maybe Text]
queryToMaybeList query list = let arr = map (\a -> join $ lookup a query) list in 
     map (\a -> case a of 
            Nothing -> Nothing 
            Just a' -> Just $ decodeUtf8 a') arr