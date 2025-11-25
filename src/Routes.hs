{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Web.Scotty
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Network.HTTP.Types.Status (status200)
import Database.PostgreSQL.Simple (Connection, Only(..), query)
import Data.Aeson
  ( FromJSON(..), Value(..), (.=), object, (.:), toJSON
  )
import Data.Aeson.Types (withObject)
import Data.Text.Lazy (unpack)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Models.Product
import Models.Category
import Models.User
import Models.Order
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

data RegisterRequest = RegisterRequest
  { rrUserName :: Text
  , rrEmail    :: Text
  , rrPassword :: Text
  } deriving Show

instance FromJSON RegisterRequest where
  parseJSON = withObject "RegisterRequest" $ \o -> do
    userName <- o .: "userName"
    email <- o .: "email"
    password <- o .: "password"
    return $ RegisterRequest userName email password

data LoginRequest = LoginRequest
  { lrUserName :: Text
  , lrPassword :: Text
  } deriving Show

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \o -> do
    userName <- o .: "userName"
    password <- o .: "password"
    return $ LoginRequest userName password


routes :: Maybe Connection -> [Category] -> [Product] -> ScottyM ()
routes conn categories allProducts = do

  -- Handle OPTIONS for CORS preflight
  options (regex ".*") $ status status200

  -- Auth routes (mock for demo mode)
  post "/api/auth/register" $ do
    case conn of
      Just c -> do
        req <- jsonData :: ActionM RegisterRequest
        let userName = rrUserName req
            email = rrEmail req
            password = rrPassword req
        userId <- liftIO $ createUser c userName email password
        json $ object ["userId" .= userId]
      Nothing -> do
        -- Demo mode - just return mock user ID
        json $ object ["userId" .= (1 :: Int)]

  post "/api/auth/login" $ do
    case conn of
      Just c -> do
        req <- jsonData :: ActionM LoginRequest
        let userName = lrUserName req
            password = lrPassword req
        userResult <- liftIO $ getUserByCredentials c userName password
        case userResult of
          [user] -> json $ object ["userId" .= userID user]
          _ -> raise "Invalid credentials"
      Nothing -> do
        -- Demo mode - accept any login
        req <- jsonData :: ActionM LoginRequest
        json $ object ["userId" .= (1 :: Int)]


  get "/api/products" $ json allProducts

  get "/api/products/search" $ do
    q <- param "q" `rescue` const (return ("" :: Text))
    let lowerQ = T.toLower q
        results = filter (\p -> lowerQ `T.isInfixOf` T.toLower (Models.Product.name p)) allProducts
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

  get "/api/products/filtered" $ do
    q <- param "q" `rescue` const (return ("" :: Text))
    categoryParam <- param "category" `rescue` const (return ("" :: Text))
    manufacturerParam <- param "manufacturer" `rescue` const (return ("" :: Text))
    inStockParam <- param "inStock" `rescue` const (return ("" :: Text))
    sortParam <- param "sort" `rescue` const (return ("" :: Text))

    let filtered = allProducts

    -- Apply search
    let filtered1 = if not (T.null q)
                    then filter (\p -> T.toLower q `T.isInfixOf` T.toLower (Models.Product.name p)) filtered
                    else filtered

    -- Apply category
    let filtered2 = if not (T.null categoryParam)
                    then filter (\p -> category_id p == read (T.unpack categoryParam)) filtered1
                    else filtered1

    -- Apply manufacturer
    let filtered3 = if not (T.null manufacturerParam)
                    then filterByManufacturer (read (T.unpack manufacturerParam)) filtered2
                    else filtered2

    -- Apply inStock
    let filtered4 = if not (T.null inStockParam)
                    then filterByStock (read (T.unpack inStockParam)) filtered3
                    else filtered3

    -- Apply sorting
    let sorted = case sortParam of
                   "price-asc" -> sortByPriceAsc filtered4
                   "price-desc" -> sortByPriceDesc filtered4
                   "name" -> sortByName filtered4
                   _ -> filtered4

    json sorted

  post "/api/orders" $ do
    req <- jsonData :: ActionM OrderRequest
    let items  = orItems req
        userId = orUserId req

    let total = calculateTotalCost items allProducts
        (final, discountDesc) = calculateFinalCost categories items allProducts total
        saved = total - final

    case conn of
      Just c -> do
        orderId <- liftIO $ createOrder c userId items total final
        json $ object
          [ "orderId"   .= orderId
          , "totalCost" .= total
          , "finalCost" .= final
          , "saved"     .= saved
          , "message"   .= ("Order placed successfully!" :: String)
          ]
      Nothing -> do
        -- Demo mode - just return mock order ID
        json $ object
          [ "orderId"   .= (999 :: Int)
          , "totalCost" .= total
          , "finalCost" .= final
          , "saved"     .= saved
          , "message"   .= ("Demo order placed successfully!" :: String)
          ]

  post "/api/cart/calculate" $ do
    req <- jsonData :: ActionM OrderRequest
    let items  = orItems req
        userId = orUserId req

    let total = calculateTotalCost items allProducts
        (final, discountDesc) = calculateFinalCost categories items allProducts total
        saved = total - final

    json $ object
      [ "totalCost" .= total
      , "finalCost" .= final
      , "saved"     .= saved
      , "discountDescription" .= discountDesc
      ]

  get "/api/orders/:userId" $ do
    userId <- read <$> param "userId"
    case conn of
      Just c -> do
        orders <- liftIO $ getOrdersByUser c userId
        json orders
      Nothing -> do
        -- Demo mode - return mock order
        let mockOrder = Order 1 userId [(1, 2), (3, 1)] (read "2023-01-01 00:00:00 UTC") 100000.0 95000.0
        json [mockOrder]