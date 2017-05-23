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

