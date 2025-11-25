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
import Data.Text (Text, isInfixOf, toLower)  -- Добавлено!
import Control.Monad.IO.Class (liftIO)
import Models.Product
import Models.Category
import Models.User
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