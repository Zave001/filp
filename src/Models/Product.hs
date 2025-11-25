{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Product where

import GHC.Generics
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)

data Product = Product
  { productID       :: Int
  , name            :: Text
  , category_id     :: Int
  , manufacturer_id :: Int
  , price           :: Scientific
  , inStock         :: Bool
  , attributes      :: Value
  } deriving (Show, Eq, Generic)

instance FromJSON Product
instance ToJSON Product