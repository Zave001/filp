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
import Logic.Filters
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

  get "/api/products/manufacturer/:id" $ do
    manId <- read <$> param "id"
    let results = filterByManufacturer manId allProducts
    json results

  get "/api/products/stock/:bool" $ do
    stock <- read <$> param "bool"
    let results = filterByStock stock allProducts
    json results

  get "/api/products/sort/price/asc" $ do
    let results = sortByPriceAsc allProducts
    json results

  get "/api/products/sort/price/desc" $ do
    let results = sortByPriceDesc allProducts
    json results

  get "/api/products/sort/name" $ do
    let results = sortByName allProducts
    json results

  get "/api/products/filter/attribute" $ do
    attr <- param "attr"
    val <- param "value"
    let results = filterByAttribute attr val allProducts
    json results

  get "/api/products/filter/numeric" $ do
    attr <- param "attr"
    target <- read <$> param "value"
    let results = filterByNumericAttribute attr target allProducts
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