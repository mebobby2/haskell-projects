{-# LANGUAGE BangPatterns #-}

module Chapter5.Problems where

import Chapter5.Infinite

--foldl (+) 0 [1 .. 1000000000]
--foldr (+) 0 [1 .. 1000000000]

--sumForce :: [Integer] -> Integer
--sumForce xs = sumForce' xs 0
--              where sumForce' [] z    = z
--                    sumForce' (y:ys) z = let s = z + y in s `seq` sumForce' ys s

sumForce :: [Integer] -> Integer
sumForce xs = sumForce' xs 0
              where sumForce' [] z    = z
                    sumForce' (y:ys) z = sumForce' ys $! (z+y)

--sumForce [1 .. 1000000000]

--There’s a GHC extension to patterns, called BangPatterns, which allows forcing the evaluation of some parts
--of the pattern. Concretely, you can write ! before any part of the pattern, and then when matching is tried
--against that pattern, the expression in that point will be evaluated up to a constructor and then the match
--will be tried. For example, you can write a function that adds all the years from a list of time machines,
--using this syntax both to force the addition of each step and to ensure that the year in each time machine
--is also evaluated (so the addition does not have a thunk as second argument):

sumYears :: [TimeMachine] -> Integer
sumYears xs = sumYears' xs 0
              where sumYears' []            z = z
                    sumYears' (TM _ !y :ys) z = let !s = z + y in sumYears' ys s

--sumYears [TM "A" 1, TM "B" 2, TM "C" 3]


