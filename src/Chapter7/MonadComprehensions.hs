{-# LANGUAGE MonadComprehensions #-}

module Chapter7.MonadComprehensions where

--purchaseValueWithDo :: Integer -> Maybe Double
--purchaseValueWithDo purchaseId = do n         <- numberItemsByPurchaseId purchaseId
--                                    productId <- productIdByPurchaseId purchaseId
--                                    price     <- priceByProductId productId
--                                    return $ fromInteger n * price

--translated into monad list comprehension syntax:

purchaseValueWithDo :: Integer -> Maybe Double
purchaseValueWithDo purchaseId =
  [ fromInteger n * price | n         <- numberItemsByPurchaseId purchaseId
                          , productId <- productIdByPurchaseId purchaseId
                          , price     <- priceByProductId productId ]


--[ price | price <- priceByProductId productId, price > 5.0 ]

--translated into monad list comprehension syntax:

do price <- priceByProductId productId
   guard price > 5.0
   return price


--[x*y | x <- [-1,1,-2], y <- [1,2,3], then sortWith by x]
--translated into monad list comprehension syntax:
do (x, y) <- sortWith (\(x,y) -> x)
                      (do x <- [-1,1,2], y <- [1,2,3], return (x,y))
   return $ x*y