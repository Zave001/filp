{-# LANGUAGE OverloadedStrings #-}

module Logic.Filters where

import Models.Product
import Models.Category
import Data.Text (Text, toLower, isInfixOf, pack, unpack)
import Data.List (sortOn, foldl')
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson (Value(..), Object)
import Data.Aeson.Key (fromString)
import Data.Scientific (toRealFloat)
import Data.Maybe (maybe)

searchByName :: Text -> [Product] -> [Product]
searchByName query = filter (\p -> toLower query `isInfixOf` toLower (Models.Product.name p))

sortByPriceAsc :: [Product] -> [Product]
sortByPriceAsc = sortOn price

sortByPriceDesc :: [Product] -> [Product]
sortByPriceDesc = reverse . sortOn price

sortByName :: [Product] -> [Product]
sortByName = sortOn Models.Product.name

getAttribute :: Value -> String -> Maybe String
getAttribute (Object obj) attrName = KM.lookup (fromString attrName) obj >>= \val ->
  pure $ case val of
    String t -> unpack t
    Number n -> show (toRealFloat n :: Float)
    Bool b -> show b
    _ -> "unknown"
getAttribute _ _ = Nothing

getNumericAttribute :: Value -> String -> Maybe Float
getNumericAttribute (Object obj) attrName = do
  num <- KM.lookup (fromString attrName) obj
  case num of
    Number n -> pure $ toRealFloat n
    _ -> Nothing
getNumericAttribute _ _ = Nothing

filterByAttribute :: String -> String -> [Product] -> [Product]
filterByAttribute attrName attrValue products =
  filter (\p -> maybe False (== attrValue)
    (getAttribute (attributes p) attrName)) products

filterByNumericAttribute :: String -> Float -> [Product] -> [Product]
filterByNumericAttribute attrName targetValue products =
  filter (\p -> maybe False (== targetValue)
    (getNumericAttribute (attributes p) attrName)) products

filterByAttributes :: [(String, String)] -> [Product] -> [Product]
filterByAttributes filters products =
  foldl' (\acc (attrName, attrValue) ->
    filterByAttribute attrName attrValue acc) products filters

filterByManufacturer :: Int -> [Product] -> [Product]
filterByManufacturer manufacturerId products =
  filter (\p -> manufacturer_id p == manufacturerId) products

filterByStock :: Bool -> [Product] -> [Product]
filterByStock inStockFlag products =
  filter (\p -> inStock p == inStockFlag) products

filterByCategory :: Int -> [Product] -> [Product]
filterByCategory categoryId products =
  filter (\p -> category_id p == categoryId) products

isGuitar :: Product -> [Category] -> Bool
isGuitar product categories =
  let guitarCategoryIds = [categoryID | Category categoryID name _ _ <- categories,
        name `elem` ["Электрогитары", "Акустические гитары"]]
  in category_id product `elem` guitarCategoryIds

isAmplifier :: Product -> [Category] -> Bool
isAmplifier product categories =
  let ampCategoryIds = [categoryID | Category categoryID name _ _ <- categories,
        name == "Усилители"]
  in category_id product `elem` ampCategoryIds

isCable :: Product -> [Category] -> Bool
isCable product categories =
  let cableCategoryIds = [categoryID | Category categoryID name _ _ <- categories,
        name == "Кабели"]
  in category_id product `elem` cableCategoryIds

isStrings :: Product -> [Category] -> Bool
isStrings product categories =
  let stringsCategoryIds = [categoryID | Category categoryID name _ _ <- categories,
        name == "Струны"]
  in category_id product `elem` stringsCategoryIds