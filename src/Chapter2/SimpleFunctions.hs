module Chapter2.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

--(+++) :: [a] -> [a] -> [a]
--lst1 +++ lst2 = if null lst1 {- check emptiness -}
--                then lst2 -- base case
--                else (head lst1) : (tail lst1 +++ lst2)

(+++) :: [a] -> [a] -> [a]
[]     +++ list2 = list2
(x:xs) +++ list2 = x:(xs +++ list2)

reverse2 :: [a] -> [a]
reverse2 list = if null list
                then []
                else reverse2 (tail list) +++ [head list]

--maxmin list = let h = head list
--              in if null (tail list)
--                 then (h, h)
--                 else ( if h > t_max then h else t_max
--                      , if h < t_min then h else t_min)
--                      where t = maxmin (tail list)
--                            t_max = fst t
--                            t_min = snd t

maxmin [x]    = (x,x)
maxmin (x:xs) = ( if x > xs_max then x else xs_max
                , if x < xs_min then x else xs_min
                ) where (xs_max, xs_min) = maxmin xs

sorted :: [Integer] -> Bool
sorted [] = True
sorted [_] = True
sorted (x : r@(y:_)) = x < y && sorted r

--ifibonacci :: Integer -> Maybe Integer
--ifibonacci n = if n < 0
--               then Nothing
--               else case n of
--                      0 -> Just 0
--                      1 -> Just 1
--                      n -> let Just f1 = ifibonacci (n-1)
--                               Just f2 = ifibonacci (n-2)
--                            in Just (f1 + f2)

ifibonacci n | n < 0 = Nothing
ifibonacci 0         = Just 0
ifibonacci 1         = Just 1
ifibonacci n | otherwise = let (Just f1, Just f2) = (ifibonacci (n-1), ifibonacci (n-2))
                           in Just (f1 + f2)

--binom _ 0 = 1
--binom x x = 1
--binom n k = (binom (n-1) (k-1)) + (binom (n-1) k)

binom _ 0          = 1
binom x y | x == y = 1
binom n k          = (binom (n-1) (k-1)) + (binom (n-1) k)

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (mod x y) == 0

--specialMultiples :: Integer -> String
--specialMultiples n | multipleOf n 2 = show n ++ " is a multiple of 2"
--specialMultiples n | multipleOf n 3 = show n ++ " is a multiple of 3"
--specialMultiples n | multipleOf n 5 = show n ++ " is a multiple of 5"
--specialMultiples n | otherwise      = show n ++ " is a beautiful number"

specialMultiples n
  | multipleOf n 2 = show n ++ " is a multiple of 2"
  | multipleOf n 3 = show n ++ " is a multiple of 3"
  | multipleOf n 5 = show n ++ " is a multiple of 5"
  | otherwise      = show n ++ " is a beautiful number"







