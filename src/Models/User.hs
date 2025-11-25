{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.User where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy.Char8 (pack)

hashPassword :: Text -> Text
hashPassword pwd = T.pack $ show $ md5 $ pack $ T.unpack pwd

data User = User
  { userID    :: Int
  , userName  :: Text
  , email     :: Text
  , password  :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON User
instance ToJSON User