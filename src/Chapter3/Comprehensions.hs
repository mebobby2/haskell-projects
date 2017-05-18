module Chapter3.Comprehensions where

import Chapter3.ParamPoly


--doubleOdds :: [Integer] -> [Integer]
--doubleOdds list = map (*2) $ filter odd list

doubleOdds :: [Integer] -> [Integer]
doubleOdds list = [ 2 * x | x <- list, odd x]

govClientNames :: [Client a] -> [String]
govClientNames listOfClients = [ clientName x | x@(GovOrg _ _) <- listOfClients ]

--multiplicationsOfFour :: [(Integer,Integer,Integer)]