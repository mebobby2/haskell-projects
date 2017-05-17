{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Chapter3.Lists where

import Data.Function (on)
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

listOfClients = [ Individual 2 (Person "H. G." "Wells")
                , GovOrg 3 "NTTF"
                , Company 4 "Wormhole Inc" (Person "Karl" "Schwarzschild") "Physicist"
                , Individual 5 (Person "Doctor" "")
                , Individual 6 (Person "Sarah" "Jane")
                ]

compareClient :: Client a -> Client a -> Ordering
compareClient (Individual{person = p1}) (Individual{person = p2}) = compare (firstName p1) (firstName p2)
compareClient (Individual {}) _ = GT
compareClient _ (Individual {}) = LT
compareClient c1 c2             = compare (clientName c1) (clientName c2)
--sortBy compareClient listOfClients


--companyDutiesAnalytics :: [Client a] -> [String]
--companyDutiesAnalytics = map (duty . head) .
--                           sortBy (\x y -> compare (length y) (length x)) .
--                           groupBy (\x y -> duty x == duty y) .
--                           filter isCompany
--                         where isCompany (Company {}) = True
--                               isCompany _            = False

companyDutiesAnalytics :: [Client a] -> [String]
companyDutiesAnalytics = map (duty . head) .
                           sortBy (flip (compare `on` length)) .
                           groupBy ((==) `on` duty) .
                           filter isCompany
                         where isCompany (Company {}) = True
                               isCompany _            = False


enum :: Int -> Int -> [Int]
enum a b | a > b = []
enum a b         = a : enum (a+1) b

--withPositions :: [a] -> [(Int, a)]
--withPositions list = zip (enum 1 $ length list) list
withPositions :: [a] -> [(Int, a)]
withPositions list = zip [1 .. length list] list

findCapital :: String -> Maybe String
findCapital country = lookup country [("France","Paris"),("Spain","Madrid"),("Portugal","Lisbon")]

--You have seen how a list can be used to represent sets (nubBy to remove duplicates) and maps (list of tuples).
--However, those implementations are very inefficient, as they require traversing a list for most operations.
--In the next chapter you will look at other containers such as those found in modules Data.Set and Data.Map.
--These other containers are especially suited for their particular uses, and have highly performant implementations.


