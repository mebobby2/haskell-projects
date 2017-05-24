module Chapter5.Infinite where

data TimeMachine = TM { manufactuer :: String, year :: Integer } deriving (Eq, Show)

timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y+1)

timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachinesFrom "Timely Inc." 100

--take 3 timelyIncMachines

--import Data.List
--find (\(TM { year = y }) -> y > 2018) timelyIncMachines



--(\list -> zip [1 .. length list] list) "abcd"
--The above is inefficient because you have to traverse the list twice: once to get its length, and once again to zip both.
--We will optimize it with a infinite list.

allNumbers :: [Integer]
allNumbers = allNumbersFrom 1

allNumbersFrom :: Integer -> [Integer]
allNumbersFrom n = n : allNumbersFrom (n+1)

--zip allNumbers "abcd"
--zip [1 .. ] "abcd"

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
--fibonacci !! 20
--the !! operator is list indexing i.e. list[20] in ruby

--fibonacci      = [0 1 1 2 3 5 8]
--tail fibonacci = [1 1 2 3 5 8 13]
--zipWith (+)    = [1 2 3 5 8 13 21]
--0 : 1 :        = [0 1 1 2 3 5 8 13 21]
--don't try to follow and understand this function as it uses recursion and a infinite list, meaning
--there is no base case for the recursion so its very hard to follow the function. Just know that to
--calculate the fibonacci sequence, for number at n, you add n-1 and n-2. In essence, this is what
--this function is doing. It shifts one of the lists one element to the left and then adds all the c
--corresponding elements at n for both list together

infinite2020Machines :: [TimeMachine]
infinite2020Machines = TM "Timely Inc." 2020 : infinite2020Machines
--infinite2020Machines !! 3
--another way to do it is using repeat combinator
--take 3 $ repeat $ TM "Timely Inc." 2020

specialOffer :: [TimeMachine]
specialOffer = cycle [TM m 2005, TM m 1994, TM m 908]
               where m = "Timely Inc."
--take 4 specialOffer

fibonacci2 :: [Integer]
fibonacci2 = map fst $ iterate (\(n,n1) -> (n1,n+n1)) (0,1)
--fibonacci2 !! 20

--seed: (0,1)
--1: (1,1)
--2: (1,2)
--3: (2,3)
--4: (3,5)
--5: (5,8)
--6: (8,13)
--7: (13,21)
--map fst: [1,1,2,3,5,8,13]

primes :: [Integer]
primes = siew [ 2 .. ]
         where siew (x:xs) = let remaining = [y | y <- xs, y `mod` x /= 0]
                              in x : siew remaining

