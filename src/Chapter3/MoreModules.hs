{-# LANGUAGE ViewPatterns #-}

module Chapter3.MoreModules(Range(),range,r,prettyRange) where

import Data.List
--import Data.List hiding (head, tail)

--import qualified Data.List (permutations, subsequence)
    --permutationsStartingWith letter = Data.List.filter (\l -> head l == letter) . Data.List.permutations

--import qualified Data.List as L
    --permutationsStartingWith letter = L.filter (\l -> head l == letter) . L.permutations

--import qualified Data.List (permutations, subsequences) as L

--import Chapter2.ParamPoly (Client()) -- only type, no constructors
--import Chapter3.ParamPoly (Client(GovOrg,Individual)) -- a subset of constructors
--import Chapter3.ParamPoly (Client(..)) -- import all constructors

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter = filter (\l -> head l == letter) . permutations

data Range = Range Integer Integer deriving Show

range :: Integer -> Integer -> Range
range a b = if a <= b then Range a b else error "a must be <= b"

data RangeObs = R Integer Integer deriving Show

r :: Range -> RangeObs
r (Range a b) = R a b

prettyRange :: Range -> String
prettyRange rng = case rng of
                    (r -> R a b) -> "[" ++ show a ++ "," ++ show b ++ "]"