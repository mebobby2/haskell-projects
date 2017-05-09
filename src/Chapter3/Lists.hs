module Chapter3.Lists where

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

