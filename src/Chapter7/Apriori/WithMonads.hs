module Chapter7.Apriori.WithMonads where

import Control.Monad (guard)
import qualified Data.Set as S
import Data.List (unfoldr)

import Chapter7.Apriori.Types

apriori :: Double -> Double -> [Transaction] -> [AssocRule]
apriori minSupport minConfidence transactions =
  generateAssocRules minConfidence transactions
    $ concat $ unfoldr (generateNextLK minSupport transactions)
                       (1, generateL1 minSupport transactions)

-- noDups removes duplicates in list
noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions =
  noDups $ do Transaction t <- transactions
              e <- S.toList t
              let fs = FrequentSet $ S.singleton e
              guard $ setSupport transactions fs > minSupport
              return fs

generateNextLK :: Double -> [Transaction] -> (Int, [FrequentSet])
                         -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLK _ _ (_, []) = Nothing
generateNextLK minSupport transactions (k, lk) =
  let lk1 = noDups $ do FrequentSet a <- lk
                        FrequentSet b <- lk
                        guard $ S.size (a `S.intersection` b) == k - 1
                        let fs = FrequentSet $ a `S.union` b
                        guard $ setSupport transactions fs > minSupport
                        return fs
   in Just (lk1, (k+1, lk1))

generateAssocRules :: Double -> [Transaction] -> [FrequentSet] -> [AssocRule]
generateAssocRules minConfidence transactions sets =
  do FrequentSet fs <- sets
     subset@(_:_) <- powerset $ S.toList fs
     let ssubset = S.fromList subset
         rule = AssocRule ssubset (fs `S.difference` ssubset)
     guard $ ruleConfidence transactions rule > minConfidence
     return rule

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)



