{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Queries where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import qualified Data.Aeson as A
import Data.Time (UTCTime)
import Data.Text (Text)
import Control.Applicative (empty)
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
  fromRow = User <$> field <*> field <*> field <*> field

instance FromRow Order where
  fromRow = do
    oid <- field
    uid <- field
    itemsVal <- field :: RowParser A.Value
    items <- case A.fromJSON itemsVal of
      A.Success x -> return x
      A.Error _ -> empty
    date <- field
    total <- field
    final <- field
    return $ Order oid uid items date total final

getAllCategories :: Connection -> IO [Category]
getAllCategories conn = query_ conn
  "SELECT CategoryID, name, description, required_attributes FROM categories ORDER BY name"

getAllProducts :: Connection -> IO [Product]
getAllProducts conn = query_ conn
  "SELECT productID, name, category_id, manufacturer_id, price, inStock, attributes FROM products WHERE inStock = true ORDER BY name"

getProductById :: Connection -> Int -> IO [Product]
getProductById conn pid = query conn
  "SELECT productID, name, category_id, manufacturer_id, price, inStock, attributes FROM products WHERE productID = ?"
  (Only pid)

searchProductsByName :: Connection -> Text -> IO [Product]
searchProductsByName conn searchTerm = query conn
  "SELECT productID, name, category_id, manufacturer_id, price, inStock, attributes \
  \FROM products WHERE name ILIKE ? AND inStock = true"
  (Only $ "%" <> searchTerm <> "%")

getProductsByCategory :: Connection -> Int -> IO [Product]
getProductsByCategory conn catId = query conn
  "SELECT productID, name, category_id, manufacturer_id, price, inStock, attributes \
  \FROM products WHERE category_id = ? AND inStock = true"
  (Only catId)

getProductsByManufacturer :: Connection -> Int -> IO [Product]
getProductsByManufacturer conn manId = query conn
  "SELECT productID, name, category_id, manufacturer_id, price, inStock, attributes \
  \FROM products WHERE manufacturer_id = ? AND inStock = true"
  (Only manId)

getProductsByStock :: Connection -> Bool -> IO [Product]
getProductsByStock conn inStock = query conn
  "SELECT productID, name, category_id, manufacturer_id, price, inStock, attributes \
  \FROM products WHERE inStock = ?"
  (Only inStock)

createUser :: Connection -> Text -> Text -> Text -> IO Int
createUser conn username email pwd = do
  [Only uid] <- query conn
    "INSERT INTO users (userName, email, password) VALUES (?, ?, ?) RETURNING userID"
    (username, email, pwd)
  return uid

createOrder :: Connection -> Int -> [OrderItem] -> Double -> Double -> IO Int
createOrder conn userId items total final = do
  [Only orderId] <- query conn
    "INSERT INTO orders (user_id, items, totalCost, finalCost) VALUES (?, ?::jsonb, ?, ?) RETURNING orderID"
    (userId, A.toJSON items, total, final)
  return orderId

getOrdersByUser :: Connection -> Int -> IO [Order]
getOrdersByUser conn userId = query conn
  "SELECT orderID, user_id, items, orderDate, totalCost, finalCost FROM orders WHERE user_id = ? ORDER BY orderDate DESC"
  (Only userId)

getUserByCredentials :: Connection -> Text -> Text -> IO [User]
getUserByCredentials conn username pwd = query conn
  "SELECT userID, userName, email, password FROM users WHERE userName = ? AND password = ?"
  (username, pwd)