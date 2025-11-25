{-# LANGUAGE OverloadedStrings #-}

module Logic.Filters where

import Models.Product
import Data.Text (Text, toLower, isInfixOf)
import Data.List (sortOn)

searchByName :: Text -> [Product] -> [Product]
searchByName query = filter (\p -> toLower query `isInfixOf` toLower (name p))

sortByPriceAsc :: [Product] -> [Product]
sortByPriceAsc = sortOn price

sortByPriceDesc :: [Product] -> [Product]
sortByPriceDesc = reverse . sortOn price

sortByName :: [Product] -> [Product]
sortByName = sortOn name