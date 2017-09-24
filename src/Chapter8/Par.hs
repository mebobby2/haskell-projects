module Chapter8.Par where

import Control.DeepSeq
import Control.Monad.Par

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n = let oneFactor = findFactor n 2
                 in oneFactor : (findFactors $ n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m | n == m         = n
               | n `mod` m == 0 = m
               | otherwise      = findFactor n (m + 1)

findTwoFactors :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactors x y = (findFactors x, findFactors y)

findTwoFactorsT :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactorsT x y = runPar $ do
  factorsXVar <- spawnP $ findFactors x
  let factorsY = findFactors y
      _        = rnf factorsY -- call to rnf from the deepseq library to fully evaluate the factorization of y
  factorsX <- get factorsXVar
  return (factorsX, factorsY)
