{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Chapter3.Lists where

import Data.List
import Chapter3.ParamPoly

data InfNumber a = MinusInfinity
                 | Number a
                 | PlusInfinity
                 deriving Show

infMax MinusInfinity x       = x
infMax x MinusInfinity       = x
infMax PlusInfinity _        = PlusInfinity
infMax _ PlusInfinity        = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)
--foldr (\x y -> infMax (Number x) y) MinusInfinity [1,2,3]

maximum' :: [Integer] -> Integer
maximum' = foldr1 max

minimumBy :: (Ord a, Ord b) => (a -> b) -> [a] -> a
minimumBy f = snd . foldr1 (\x y -> min x y) . map (\x -> (f x, x))
--minimumBy (\x -> -x) [1,2,3]

--bothFilters :: (a -> Bool) -> [a] -> ([a],[a])
--bothFilters p list = (filter p list, filter (not . p) list)
--Above implementation is naive because it traverses the list twice
--Instead, can use funcions already defined in Data.List
--partition (> 0) [1,2,-3,4,-5,-6]
--find (> 0) [1,2,-3,4,-5,-6]

skipUntilGov :: [Client a] -> [Client a]
skipUntilGov = dropWhile (\case { GovOrg {} -> False ; _ -> True })

isIndividual :: Client a -> Bool
isIndividual (Individual {}) = True
isIndividual _               = False

checkIndividualAnalytics :: [Client a] -> (Bool, Bool)
checkIndividualAnalytics cs = (any isIndividual cs, not $ all isIndividual cs)

