{-# LANGUAGE TransformListComp, RecordWildCards, ParallelListComp #-}

module Chapter3.Comprehensions where

import Chapter3.ParamPoly
import Data.Char
import GHC.Exts


--doubleOdds :: [Integer] -> [Integer]
--doubleOdds list = map (*2) $ filter odd list

doubleOdds :: [Integer] -> [Integer]
doubleOdds list = [ 2 * x | x <- list, odd x]

govClientNames :: [Client a] -> [String]
govClientNames listOfClients = [ clientName x | x@(GovOrg _ _) <- listOfClients ]

multiplicationsOfFour :: [(Integer,Integer,Integer)]
multiplicationsOfFour = [(x,y,x*y) | x <- [1 .. 4], y <- [1 .. 10]]

dominos :: [(Integer, Integer)]
dominos = [(x,y) | x <- [0 .. 6], y <- [x .. 6]]


upperString = [ toUpper c | s <- ["This", "is", "a", "list"], c <- ' ':s]

vectorNorms = [ sqrt v | (x,y) <- [(1,2),(3,8)], let v = x*x+y*y]

dominosWithGuard = [(x,y) | x <- [1 .. 6], y <- [1 .. 6], x <= y]

multipleReverse = [x*y | x <- [-1,1,-2], y <- [1,2,3], then reverse]

sortList = [x*y | x <- [-1,1,-2], y <- [1,2,3], then sortWith by x]

--sortGroupList = [(the p, m) | x <- [-1,1,-2]
--                            , y <- [1,2,3]
--                            , let m = x*y
--                            , let p = m > 0,
--                            , then group by p using groupWith ]

companyAnalytics :: [Client a] -> [(String, [(Person, String)])]
companyAnalytics clients = [ (the clientName, zip person duty)
                           | client@(Company { .. }) <- clients
                           , then sortWith by duty
                           , then group by clientName using groupWith
                           , then sortWith by length client
                           ]

traditionalNesting = [ x*y | x <- [1,2,3], y <- [1,2,3]]
zipping = [ x*y | x <- [1,2,3] | y <- [1,2,3]]

