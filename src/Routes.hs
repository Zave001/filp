{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Web.Scotty
import Database.PostgreSQL.Simple (Connection, Only(..), query)
import Data.Aeson
  ( FromJSON(..), Value(..), (.=), object, (.:), toJSON
  )
import Data.Aeson.Types (withObject)
import Data.Text.Lazy (unpack)
import Data.Text (isInfixOf, toLower)  -- Добавлено!
import Control.Monad.IO.Class (liftIO)
import Models.Product
import Models.Category
import Logic.Discount
import Database.Queries

data OrderRequest = OrderRequest
  { orItems  :: [(Int, Int)]
  , orUserId :: Int
  } deriving Show

instance FromJSON OrderRequest where
  parseJSON = withObject "OrderRequest" $ \o -> do
    itemsArr <- o .: "items"
    items <- traverse parseItem itemsArr
    userId <- o .: "userId"
    return $ OrderRequest items userId
    where
      parseItem = withObject "Item" $ \i -> (,) <$> i .: "productId" <*> i .: "quantity"

routes :: Connection -> [Category] -> [Product] -> ScottyM ()
routes conn categories allProducts = do

  get "/api/products" $ json allProducts

  get "/api/products/search" $ do
    q <- param "q" `rescue` const (return "")
    let lowerQ = toLower q
        results = filter (\p -> lowerQ `isInfixOf` toLower (Models.Product.name p)) allProducts
    json results

  get "/api/products/category/:id" $ do
    catId <- read <$> param "id"
    let results = filter (\p -> category_id p == catId) allProducts
    json results

  post "/api/orders" $ do
    req <- jsonData :: ActionM OrderRequest
    let items  = orItems req
        userId = orUserId req

    let total = calculateTotalCost items allProducts
        final = calculateFinalCost categories items allProducts total
        saved = total - final

    orderId <- liftIO $ createOrder conn userId items total final

    json $ object
      [ "orderId"   .= orderId
      , "totalCost" .= total
      , "finalCost" .= final
      , "saved"     .= saved
      , "message"   .= ("Order placed successfully!" :: String)
      ]