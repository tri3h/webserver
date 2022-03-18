{-# LANGUAGE OverloadedStrings #-}
module Utility where

import Prelude hiding (splitOn, concat,words, breakOn,dropWhile, takeWhile, drop)
import Data.ByteString (ByteString, breakSubstring, dropWhile, takeWhile, append, drop)
import Data.ByteString.Internal (c2w)
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

getPicture :: ByteString -> ByteString -> Either Text Text
getPicture pict name = let check = snd $ breakSubstring ("name=\"" `append` name) pict
    in case check of
            "" -> Left "No picture with such name"
            _ -> Right $ decodeUtf8 ( takeWhile (/=c2w '-') $ drop 3 $ dropWhile (/=c2w '\n') (snd $ breakSubstring "Content-Type:" check))