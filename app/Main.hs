{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Database.Connection (connectLocal)
import Database.Queries
  ( getAllCategories
  , getAllProducts
  , searchProductsByName
  , getProductsByCategory
  , getProductsByManufacturer
  , getProductsByStock
  , createUser
  , getUserByCredentials
  , createOrder
  )
import Routes (routes)
import Models.Category
import Models.Product
import Data.Text.Lazy (unpack)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Control.Exception (try, SomeException)

runServer :: [Category] -> [Product] -> IO ()
runServer categories products = do
  putStrLn "MusicalHub API started successfully!"
  putStrLn "Open: http://localhost:8080"
  putStrLn "Examples:"
  putStrLn "  -> POST /api/auth/register (registration)"
  putStrLn "  -> POST /api/auth/login (login)"
  putStrLn "  -> GET /api/products"
  putStrLn "  -> GET /api/products/search?q=yamaha"
  putStrLn "  -> GET /api/products/category/1"
  putStrLn "  -> POST /api/orders (with JSON cart)\n"

  -- Запускаем веб-сервер на порту 8080 с CORS
  scotty 8080 $ do
    -- CORS middleware для фронтенда
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
      { corsOrigins = Just (["http://localhost:5173"], False)  -- Allow frontend origin
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Content-Type", "Authorization"]
      }
    -- Передаём демо-данные в роуты
    routes Nothing categories products

-- Main entry point
main :: IO ()
main = do
  putStrLn "Starting MusicalHub - online musical instruments store"
  putStrLn "Connecting to database..."

  -- Подключаемся к PostgreSQL
  connResult <- try connectLocal
  case connResult of
    Left ex -> do
      putStrLn "Failed to connect to database. Using demo data."
      putStrLn $ "Error: " ++ show (ex :: SomeException)
      -- Демо-данные для тестирования
      let categories = [
            Category 1 "Электрогитары" (Just "Электрогитары и бас-гитары") "{\"body_wood\": \"string\", \"neck_wood\": \"string\"}",
            Category 2 "Акустические гитары" (Just "Акустические и классические гитары") "{\"body_wood\": \"string\", \"top_wood\": \"string\"}",
            Category 3 "Усилители" (Just "Гитарные усилители") "{\"power_w\": \"int\", \"amp_type\": \"string\"}"
            ]
          products = [
            Product 1 "Yamaha Pacifica 112V" 1 1 42990.00 True "{\"body_wood\": \"Alder\", \"neck_wood\": \"Maple\"}",
            Product 2 "Fender Stratocaster" 1 7 55990.00 True "{\"body_wood\": \"Alder\", \"neck_wood\": \"Maple\"}",
            Product 3 "Marshall DSL40CR" 3 14 45990.00 True "{\"power_w\": 40, \"amp_type\": \"Tube\"}",
            Product 4 "Taylor 114ce" 2 10 89990.00 True "{\"body_wood\": \"Walnut\", \"top_wood\": \"Spruce\"}"
            ]
      putStrLn $ "Using demo data: " ++ show (length categories) ++ " categories, " ++ show (length products) ++ " products"
      runServer categories products
    Right conn -> do
      -- Load categories once at start (needed for name-based discounts!)
      categories <- getAllCategories conn
      putStrLn $ "Loaded categories: " ++ show (length categories)

      -- Load products for cache (optional, can be removed)
      products <- getAllProducts conn
      putStrLn $ "Loaded products: " ++ show (length products)
      runServer categories products