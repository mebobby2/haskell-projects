{-# LANGUAGE LambdaCase #-}

module Chapter3.FnsParams where

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f (x + 2)

equalTuples :: [(Integer, Integer)] -> [Bool]
equalTuples t = map (\(x,y) -> x == y) t

sayHello :: [String] -> [String]
sayHello names = map (\case "Bobby" -> "Hello, writer"
                            name    -> "Welcome, " ++ name
                     ) names

--map (multiplyByN 5) [1,2,3]
multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = \x -> n*x

--filter (filterANumber 2) [1,2,3,4,2,5]
filterANumber :: Integer -> (Integer -> Bool)
filterANumber n = \x -> x == n

filterNot :: (Integer -> Bool) -> [Integer] -> [Integer]
filterNot f [item] =  if not (f item) then [item] else []
filterNot f (x:xs) = filterNot f [x] ++ filterNot f xs
