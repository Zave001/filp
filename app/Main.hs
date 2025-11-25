{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
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
import Data.Text.Lazy (unpack)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

-- Главная точка входа
main :: IO ()
main = do
  putStrLn "Запуск MusicalHub - онлайн-магазин музыкальных инструментов"
  putStrLn "Подключение к базе данных..."

  -- Подключаемся к PostgreSQL
  conn <- connectLocal

  -- Загружаем категории один раз при старте (нужно для скидок по имени!)
  categories <- getAllCategories conn
  putStrLn $ "Загружено категорий: " ++ show (length categories)

  -- Загружаем товары для кэша (по желанию, можно убрать)
  products <- getAllProducts conn
  putStrLn $ "Загружено товаров: " ++ show (length products)

  putStrLn "MusicalHub API успешно запущен!"
  putStrLn "Открывайте: http://localhost:8080"
  putStrLn "Примеры:"
  putStrLn "  -> POST /api/auth/register (регистрация)"
  putStrLn "  -> POST /api/auth/login (вход)"
  putStrLn "  -> GET /api/products"
  putStrLn "  -> GET /api/products/search?q=yamaha"
  putStrLn "  -> GET /api/products/category/1"
  putStrLn "  -> POST /api/orders (с JSON-корзиной)\n"

  -- Запускаем веб-сервер на порту 8080
  scotty 8080 $ do
    -- Передаём соединение и категории в роуты
    routes conn categories products