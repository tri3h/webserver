{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Image where

import Data.Text 
import GHC.Generics
import Data.Aeson
import Database.PostgreSQL.Simple.ToField

data Image = Image Text | Link Text
    deriving (Show, Read, Generic)

type ImageId = Integer

instance ToJSON Image where 
    toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue}

instance FromJSON Image where 
    parseJSON = withObject "Image" $ \obj -> do 
        avatar <- obj .:? "avatar"
        case avatar of 
            Nothing -> do 
                    mainPhoto <- obj .:? "main_photo"
                    case mainPhoto of 
                        Nothing -> Image <$> obj .: "minor_photo"
                        Just x -> return $ Image x
            Just x -> return $ Image x

instance ToField Image where 
    toField (Image x) = toField x 
    toField (Link x) = toField x
