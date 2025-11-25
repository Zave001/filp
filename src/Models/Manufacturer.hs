{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Manufacturer where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

data Manufacturer = Manufacturer
  { manufacturerID :: Int
  , name           :: Text
  , country        :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Manufacturer
instance ToJSON Manufacturer