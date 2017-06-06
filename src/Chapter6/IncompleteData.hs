module Chapter6.IncompleteData where

import Data.Maybe

purchasesByClientId :: Integer -> [Integer]
purchasesByClientId _ = [1,2,3]

numberItemsByPurchaseId :: Integer -> Maybe Integer
numberItemsByPurchaseId _ = Just 4

productIdByPurchaseId :: Integer -> Maybe Integer
productIdByPurchaseId _ = Just 4

priceByProductId :: Integer -> Maybe Double
priceByProductId _ = Just 100.0

meanPurchase :: Integer -- the client identifier
             -> Double  -- the mean purchase
meanPurchase clientId = let p = purchasesByClientId clientId
                         in foldr (+) 0.0 $ catMaybes $ map purchaseValue p

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
  case numberItemsByPurchaseId purchaseId of
    Nothing -> Nothing
    Just n  -> case productIdByPurchaseId purchaseId of
                 Nothing -> Nothing
                 Just productId -> case priceByProductId productId of
                                     Nothing -> Nothing
                                     Just price -> Just $ (fromInteger n) * price
