{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.User where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

data User = User
  { userID    :: Int
  , userName  :: Text
  , email     :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON User
instance ToJSON User