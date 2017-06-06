{-# LANGUAGE TypeSynonymInstances, LiberalTypeSynonyms #-}

module Chapter6.CombinatorsState where

import Chapter6.Vector

import Data.Default
import Data.List
import qualified Data.Map as M

type State s a = s -> (a, s)

--thenDo :: (s -> (a,s)) -> (a -> s -> (b,s)) -> s -> (b,s)
--thenDo f g s = let (resultOfF, stateAfterF) = f s
--                in g resultOfF stateAfterF

thenDo :: State s a -> (a -> State s b) -> State s b
thenDo f g = uncurry g . f
-- We know function f returns a tuple (a,s). Function g takes two
-- arguments, a and s. If we uncurry g, then it will take one
-- argument, (a,s). Since uncurry g can take the return value of
-- function f, we can compose them.

data KMeansState v = KMeansState { centroids :: [v]
                                 , threshold :: Double
                                 , steps :: Int }

newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids = M.elems . fmap (centroid . map toVector)
-- How to read this function?
-- It's using point-free style. It takes one argument, let's call it m, which is a map.
-- Example m: [("a", [(1,1), (2,2)]), ("b", [(3,3),(4,4)])]
-- Application steps:
-- 1. fmap (centroid . map toVector) m
-- 2. fmap loops through all the values of m, first value is [(1,1), (2,2)]
-- 3. (centroid . map toVector) [(1,1), (2,2)]
-- 4. map will loop through each tuple, first value is (1,1)
-- 5. toVector (1,1), then toVector (2,2)
-- 6. map toVector will return another list
-- 7. centroid list = will call apply the list returned in step 6 to the function centroid
-- 8. fmap then contines through all the values of m
-- 9. After step 9, m will look like [("a", (1,2)), ("b", (3,4))]
-- 10. M.elems [("a", (1,2)), ("b", (3,4))] will return [(1,2), (3,4)]
