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

--purchaseValue :: Integer -> Maybe Double
--purchaseValue purchaseId =
--  case numberItemsByPurchaseId purchaseId of
--    Nothing -> Nothing
--    Just n  -> case productIdByPurchaseId purchaseId of
--                 Nothing -> Nothing
--                 Just productId -> case priceByProductId productId of
--                                     Nothing -> Nothing
--                                     Just price -> Just $ (fromInteger n) * price

-- Using a combinator
thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing _ = Nothing
thenDo (Just x) f = f x

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
  numberItemsByPurchaseId purchaseId `thenDo` (\n ->
    productIdByPurchaseId purchaseId `thenDo` (\productId ->
    priceByProductId productId       `thenDo` (\price ->
    Just $ fromInteger n * price      )))

purchaseValueMonad :: Integer -> Maybe Double
purchaseValueMonad purchaseId =
  numberItemsByPurchaseId purchaseId >>= (\n ->
    productIdByPurchaseId purchaseId >>= (\productId ->
    priceByProductId productId       >>= (\price ->
    return $ fromInteger n * price      )))

purchaseValueWithDo :: Integer -> Maybe Double
purchaseValueWithDo purchaseId = do n         <- numberItemsByPurchaseId purchaseId
                                    productId <- productIdByPurchaseId purchaseId
                                    price     <- priceByProductId productId
                                    return $ fromInteger n * price

