{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Category where

import GHC.Generics
import Data.Aeson (Value, FromJSON, ToJSON)
import Data.Text (Text)

data Category = Category
  { categoryID          :: Int
  , name                :: Text
  , description         :: Maybe Text
  , required_attributes :: Value
  } deriving (Show, Eq, Generic)

instance FromJSON Category
instance ToJSON Category