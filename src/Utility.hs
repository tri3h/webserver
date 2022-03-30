{-# LANGUAGE OverloadedStrings #-}
module Utility where

import Prelude hiding (null)
import Data.Text ( Text, append, unpack, splitOn, null, unpack )
import Types.Image ( Image(..) )
import Network.HTTP.Types.URI ( QueryText )
import Control.Monad ( join )
import Data.ByteString(ByteString)
import Data.Aeson
import Types.Draft ( Description(..) )

getText :: QueryText -> Text -> Either Text Text
getText query name = case join $ lookup name query of
    Nothing -> err name
    Just x -> if null x
        then err name
        else Right x

getInteger :: QueryText -> Text -> Either Text Integer
getInteger query name = case join $ lookup name query of
    Nothing -> err name
    Just x -> if null x
        then err name
        else Right (read $ unpack x :: Integer)

getIntegers :: QueryText -> Text -> Either Text [Integer]
getIntegers query name = case join $ lookup name query of
    Nothing -> err name
    Just xs -> if null xs
        then err name
        else Right $ map (\x -> read $ unpack x :: Integer) (splitOn "," xs)

getImage :: ByteString -> Either Text Image
getImage bs = case decodeStrict bs :: (Maybe Image) of
    Nothing -> Left "No image specified"
    Just x -> Right x

getDescription :: ByteString -> Either Text Description
getDescription bs = case decodeStrict bs :: (Maybe Description) of
    Nothing -> Left "No description specified"
    Just x -> Right x

getMaybeText :: QueryText -> Text -> Maybe Text
getMaybeText query name = case join $ lookup name query of 
    Nothing -> Nothing 
    Just x -> if null x then Nothing else Just x

getMaybeInteger :: QueryText -> Text -> Maybe Integer
getMaybeInteger query name = case join $ lookup name query of 
    Nothing -> Nothing 
    Just x -> if null x then Nothing else Just (read $ unpack x :: Integer)

getMaybeIntegers :: QueryText -> Text -> Maybe [Integer]
getMaybeIntegers query name = case join $ lookup name query of 
    Nothing -> Nothing 
    Just xs -> if null xs 
        then Nothing 
        else Just $ map (\x -> read $ unpack x :: Integer) $ splitOn "," xs

getMaybeImage :: ByteString -> Maybe Image
getMaybeImage bs = case decodeStrict bs :: (Maybe Image) of 
    image@(Just (Image x)) -> if null x then Nothing else image
    _ -> Nothing 

getMaybeDescription :: ByteString -> Maybe Description
getMaybeDescription bs = case decodeStrict bs :: (Maybe Description) of 
    descr@(Just (Description x)) -> if null x then Nothing else descr
    _ -> Nothing 

err name = Left $ "No " `append` name `append` " specified"