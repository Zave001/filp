{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Queries where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson (Value, toJSON)
import Data.Time (UTCTime)
import Data.Text (Text)
import Models.Product
import Models.Category
import Models.Manufacturer
import Models.User
import Models.Order

instance FromRow Product where
  fromRow = Product <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Category where
  fromRow = Category <$> field <*> field <*> field <*> field

instance FromRow Manufacturer where
  fromRow = Manufacturer <$> field <*> field <*> field

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance FromRow Order where
  fromRow = Order <$> field <*> field <*> field <*> field <*> field <*> field

getAllCategories :: Connection -> IO [Category]
getAllCategories conn = query_ conn
  "SELECT id, name, description, required_attributes FROM categories ORDER BY name"

getAllProducts :: Connection -> IO [Product]
getAllProducts conn = query_ conn
  "SELECT id, name, category_id, manufacturer_id, price, in_stock, attributes FROM products WHERE in_stock = true ORDER BY name"

getProductById :: Connection -> Int -> IO [Product]
getProductById conn pid = query conn
  "SELECT id, name, category_id, manufacturer_id, price, in_stock, attributes FROM products WHERE id = ?"
  (Only pid)

searchProductsByName :: Connection -> Text -> IO [Product]
searchProductsByName conn q = query conn
  "SELECT id, name, category_id, manufacturer_id, price, in_stock, attributes \
  \FROM products WHERE name ILIKE ? AND in_stock = true"
  (Only $ "%" <> q <> "%")

getProductsByCategory :: Connection -> Int -> IO [Product]
getProductsByCategory conn catId = query conn
  "SELECT id, name, category_id, manufacturer_id, price, in_stock, attributes \
  \FROM products WHERE category_id = ? AND in_stock = true"
  (Only catId)

createUser :: Connection -> Text -> Text -> IO Int
createUser conn username email = do
  [Only uid] <- query conn
    "INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
    (username, email)
  return uid

createOrder :: Connection -> Int -> [OrderItem] -> Float -> Float -> IO Int
createOrder conn userId items total final = do
  [Only oid] <- query conn
    "INSERT INTO orders (user_id, items, total_cost, final_cost) VALUES (?, ?::jsonb, ?, ?) RETURNING id"
    (userId, toJSON items, total, final)
  return oid

getOrdersByUser :: Connection -> Int -> IO [Order]
getOrdersByUser conn userId = query conn
  "SELECT id, user_id, items, order_date, total_cost, final_cost FROM orders WHERE user_id = ? ORDER BY order_date DESC"
  (Only userId)