{-# LANGUAGE OverloadedStrings #-}

module Logic.Discount where

import Models.Product
import Models.Category
import Logic.Filters (isGuitar, isAmplifier, isCable, isStrings)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)


calculateTotalCost :: [(Int, Int)] -> [Product] -> Float
calculateTotalCost items products =
  let productMap = HM.fromList [(productID p, price p) | p <- products]
  in sum [fromMaybe 0.0 (HM.lookup pid productMap) * fromIntegral qty
         | (pid, qty) <- items, qty > 0]

applyComboDiscount :: [Category] -> [(Int, Int)] -> [Product] -> Float -> Float
applyComboDiscount cats items products total =
  let productMap = HM.fromList [(productID p, p) | p <- products]
      hasGuitar = any (\(pid,qty) -> qty > 0 && maybe False (\p -> isGuitar p cats) (HM.lookup pid productMap)) items
      hasAmp    = any (\(pid,qty) -> qty > 0 && maybe False (\p -> isAmplifier p cats) (HM.lookup pid productMap)) items
      hasCable  = any (\(pid,qty) -> qty > 0 && maybe False (\p -> isCable p cats) (HM.lookup pid productMap)) items
  in if hasGuitar && hasAmp && hasCable then total * 0.95 else total

applyStringsDiscount :: [Category] -> [(Int, Int)] -> [Product] -> Float -> Float
applyStringsDiscount cats items products total =
  let productMap = HM.fromList [(productID p, p) | p <- products]
      hasGuitar = any (\(pid,qty) -> qty > 0 && maybe False (\p -> isGuitar p cats) (HM.lookup pid productMap)) items
      stringsCost = sum [price p * fromIntegral qty
                        | (pid, qty) <- items, qty > 0
                        , Just p <- [HM.lookup pid productMap]
                        , isStrings p cats]
      discount = if hasGuitar && stringsCost > 0 then stringsCost * 0.10 else 0
  in total - discount

calculateFinalCost :: [Category] -> [(Int, Int)] -> [Product] -> Float -> Float
calculateFinalCost cats items products total =
  let afterCombo = applyComboDiscount cats items products total
  in applyStringsDiscount cats items products afterCombo