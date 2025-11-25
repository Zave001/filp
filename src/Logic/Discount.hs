{-# LANGUAGE OverloadedStrings #-}

module Logic.Discount where

import Models.Product
import Models.Category
import Logic.Filters (isGuitar, isAmplifier, isCable, isStrings)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)


calculateTotalCost :: [(Int, Int)] -> [Product] -> Double
calculateTotalCost items products =
  let productMap = HM.fromList [(productID p, realToFrac (price p)) | p <- products]
  in sum [fromMaybe 0.0 (HM.lookup pid productMap) * fromIntegral qty
         | (pid, qty) <- items, qty > 0]

applyComboDiscount :: [Category] -> [(Int, Int)] -> [Product] -> Double -> Double
applyComboDiscount cats items products total =
  let productMap = HM.fromList [(productID p, p) | p <- products]
      hasGuitar = any (\(pid,qty) -> qty > 0 && maybe False (\p -> isGuitar p cats) (HM.lookup pid productMap)) items
      hasAmp    = any (\(pid,qty) -> qty > 0 && maybe False (\p -> isAmplifier p cats) (HM.lookup pid productMap)) items
      hasCable  = any (\(pid,qty) -> qty > 0 && maybe False (\p -> isCable p cats) (HM.lookup pid productMap)) items
  in if hasGuitar && hasAmp && hasCable then total * 0.95 else total

applyStringsDiscount :: [Category] -> [(Int, Int)] -> [Product] -> Double -> Double
applyStringsDiscount cats items products total =
  let productMap = HM.fromList [(productID p, p) | p <- products]
      hasGuitar = any (\(pid,qty) -> qty > 0 && maybe False (\p -> isGuitar p cats) (HM.lookup pid productMap)) items
      stringsCost = sum [realToFrac (price p) * fromIntegral qty
                        | (pid, qty) <- items, qty > 0
                        , Just p <- [HM.lookup pid productMap]
                        , isStrings p cats]
      discount = if hasGuitar && stringsCost > 0 then stringsCost * 0.10 else 0
  in total - discount

calculateFinalCost :: [Category] -> [(Int, Int)] -> [Product] -> Double -> (Double, String)
calculateFinalCost cats items products total =
  let afterCombo = applyComboDiscount cats items products total
      final = applyStringsDiscount cats items products afterCombo
      comboApplied = afterCombo < total
      stringsApplied = final < afterCombo
      desc = case (comboApplied, stringsApplied) of
               (True, True) -> "Комбо скидка 5% + скидка на струны 10%"
               (True, False) -> "Комбо скидка 5%"
               (False, True) -> "Скидка на струны 10%"
               _ -> "Без скидки"
  in (final, desc)