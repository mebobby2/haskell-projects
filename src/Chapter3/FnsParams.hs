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

--double list = map (\x -> x * 2) list
--double      = \list -> map (\x -> x * 2) list
double = map (\x -> x * 2)

--duplicateOdds list = map (*2) $ filter odd list
duplicateOdds = map (*2) . filter odd

--(uncurry max) (3,2)
--map (uncurry max) [(1,2),(2,1),(3,4)]
uncurry2 :: (a -> b -> c) -> (a,b) -> c
uncurry2 f = \(x,y) -> f x y

curry2 :: ((a,b) -> c) -> a -> b -> c
curry2 f = \x y -> f (x, y)

--(sqrt *** atan) (4, 3)
(***) :: (a -> b) -> (c -> d) -> ((a,c) -> (b,d))
f *** g = \(x,y) -> (f x, g y)

--(sqrt *** atan) (duplicate 4)
duplicate :: a -> (a,a)
duplicate x = (x,x)


--3x + 7(x + 2)
formula1 :: Integer -> Integer
formula1 = uncurry2 (+) . ( ((*7) . (+2)) *** (*3) ) . duplicate


