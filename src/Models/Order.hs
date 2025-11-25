{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Order where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Time (UTCTime)

type OrderItem = (Int, Int)

data Order = Order
  { orderID     :: Int
  , user_id     :: Int
  , items       :: [OrderItem] 
  , orderDate   :: UTCTime
  , totalCost   :: Float
  , finalCost   :: Float
  } deriving (Show, Eq, Generic)

instance FromJSON Order
instance ToJSON Order